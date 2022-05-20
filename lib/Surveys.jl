module Surveys

using Dates
using Genie.Renderers.Html
import Base: show, isvalid, isempty

export Survey, SurveyPart, Question,
    Answer, Response, update!, clear!,
    nonempty, wordlimit, charlimit,
    SurveyID, ResponseID,
    Checkbox, TextInput, DateInput, NumberInput, IntegerInput,
    TextArea, Dropdown, RadioSelect, MultiSelect, RangeSelect

# ---------------------
# Main Types
# ---------------------

# Input Field

abstract type FormField{T} end

# Question

struct Question{F <: FormField}
    id::Symbol
    prompt::AbstractString
    field::F
    validators::Vector{Function}
    postprocessors::Vector{Function}
    function Question(id::Symbol, prompt::AbstractString,
                      field::F, validators::Vector{Function},
                      postprocessors::Vector{Function}) where {F<:FormField}
        if id in (:uid, :page)
            @warn "Question uses a reserved id $id, this is likely to cause issues"
        end
        postprocessors = map(p -> if p == last; lastsoftfail else p end, postprocessors)
        new{F}(id, prompt, field, validators, postprocessors)
    end
end

function nonempty(value::AbstractString)
    if ismissing(value) || (isa(value, AbstractString) && isempty(strip(value)))
        "Must be answered"
    end
end

function nonempty(values::Vector{<:AbstractString})
    if length(values) == 0 || all(isempty, strip.(values))
        "Must be answered"
    end
end

nonempty(::Bool) = nothing
nonempty(::Number) = nothing
nonempty(::Date) = nothing

lastsoftfail(v::Vector) =
    if length(v) > 0
        last(v)
    else "" end

wordlimit(min::Int, max::Int) = function(text::AbstractString)
    wordcount = length(split(text))
    if wordcount < min
        string("Need at least ", min, if min > 1 " words" else " word" end,
               " (currently ", wordcount, ")")
    elseif wordcount > max
        string("No more than ", max, if max > 1 " words" else " word" end,
               " are permitted (currently ", wordcount, ")")
    end
end
wordlimit(max::Int) = wordlimit(0, max)

charlimit(min::Int, max::Int) = function(text::AbstractString)
    charcount = length(split(text))
    if charcount < min
        string("Need at least ", min, if min > 1 " characters" else " character" end,
               " (currently ", charcount, ")")
    elseif charcount > max
        string("No more than ", max, if max > 1 " characters" else " character" end,
               " are permitted (currently ", charcount, ")")
    end
end
charlimit(max::Int) = charlimit(0, max)

default_postprocessors(::FormField) = Function[last, strip]
default_validators(::FormField) = Function[]

function Question(id::Symbol, prompt::AbstractString, field::FormField;
                  postprocessors::Union{Function, Vector{<:Function}} =
                      default_postprocessors(field),
                  validators::Union{Function, Vector{<:Function}} =
                      default_validators(field),
                  mandatory::Bool = true)
    fullvalidators = if mandatory
        vcat(nonempty, validators)
    else
        vcat(validators)
    end |> Vector{Function}
    Question(id, prompt, field, fullvalidators, postprocessors)
end

function prompttoid(prompt::String)
    prompt |>
    p -> replace(p, r"[^A-Za-z0-9\s]" => "") |>
         p -> replace(p, r"\s+" => "_") |>
              lowercase |>
              Symbol
end

Question(prompt::AbstractString, field::FormField; kargs...) =
    Question(prompttoid(prompttoid), prompt, field; kargs...)

# Field-based question constructors

function (F::Type{<:FormField})(id::Symbol, prompt::AbstractString,
                                args...; kwargs...)
    question_extra_kwargs = (:postprocessors, :validators, :mandatory)
    question_kwargs = filter(kw -> kw.first ∈ question_extra_kwargs, kwargs)
    field_kwargs = filter(kw -> kw.first ∉ question_extra_kwargs, kwargs)
    try
        Question(id, prompt, F(args...; field_kwargs...); question_kwargs...)
    catch e
        print(stderr, "\nError while processing question $id\n")
        rethrow(e)
    end
end
function (F::Type{<:FormField})(prompt::AbstractString, args...; kwargs...)
    F(prompttoid(prompt), prompt, args...; kwargs...)
end

# Survey Part

struct SurveyPart
    label::Union{AbstractString, Nothing}
    description::Union{AbstractString, Nothing}
    questions::Vector{Question}
end

Base.getindex(p::SurveyPart, id::Symbol) = findfirst(q -> q.id == id, p.questions)
Base.getindex(p::SurveyPart, index::Integer) = p.questions[index]

Base.length(p::SurveyPart) = length(p.questions)

SurveyPart(label::Union{AbstractString, Nothing}, description::Union{AbstractString, Nothing}, questions::Question...) =
    SurveyPart(label, description, questions |> collect)
SurveyPart(label::Union{AbstractString, Nothing}, questions::Question...) =
    SurveyPart(label, nothing, questions |> collect)
SurveyPart(questions::Question...) =
    SurveyPart(nothing, nothing, questions |> collect)

# Survey

const SurveyID = UInt32

struct Survey
    id::SurveyID
    name::AbstractString
    description::Union{AbstractString, Nothing}
    parts::Vector{Pair{Tuple{Union{AbstractString, Nothing}, Union{AbstractString, Nothing}}, Vector{Symbol}}}
    questions::Dict{Symbol, Question}
    function Survey(name::AbstractString,
        description::Union{AbstractString, Nothing},
        parts::Vector{<:Pair{<:Any, <:Vector{Symbol}}},
        questions::Dict{Symbol, Question})
        # Create an id that only depends on:
        # 1. Question IDs
        # 2. Question field types
        # These are the two essential components to hash, as the database interactions
        # rely on the assumption that these two components are stable.
        # Hopefully memhashing a Tuple of Symbols and Strings is somewhat stable,
        # I checked this on Julia 1.3 and 1.6 and it looked alright.
        function qhash(q::Question{<:FormField{T}}) where {T}
            hash((q.id, string(T)))
        end
        id = xor(map(qhash, values(questions))...) |>
             h -> xor(reinterpret(SurveyID, [h])...)
        typedparts = parts |> Vector{Pair{Tuple{Union{AbstractString, Nothing}, Union{AbstractString, Nothing}}, Vector{Symbol}}}
        new(id, name, description, typedparts, questions)
    end
end

Base.getindex(s::Survey, id::Symbol) = s.questions[id]
Base.getindex(s::Survey, part::Integer) =
    SurveyPart(s.parts[part].first[1], s.parts[part].first[2],
               getindex.(Ref(s.questions), s.parts[part].second))

Base.length(s::Survey) = length(s.parts)

Survey(name::AbstractString,
       description::Union{AbstractString, Nothing},
       parts::SurveyPart...) =
           Survey(name, description,
                  map(parts) do p
                      (p.label, p.description) => getfield.(p.questions, :id)
                  end |> collect,
                  Dict(q.id => q for q in
                           Iterators.flatten(getfield.(parts, :questions))))
Survey(name::AbstractString, parts::SurveyPart...) =
    Survey(name, nothing, parts...)

Survey(name::AbstractString,
       description::Union{AbstractString, Nothing},
       questions::Question...) =
           Survey(name, description,
                  [nothing => getfield.(questions, :id) |> collect],
                  Dict(q.id => q for q in questions))
Survey(name::AbstractString, questions::Question...) =
    Survey(name, nothing, questions...)

# Answer to a Question

struct Answer{T}
    value::Union{T, Missing}
    error::Union{AbstractString, Nothing}
end

Answer{T}() where {T <: Any} = Answer{T}(missing, nothing)
isempty(a::Answer) = ismissing(a.value)
isempty(a::Answer{<:AbstractString}) = ismissing(a.value) || isempty(a.value)

# Survey Response

const ResponseID = UInt32

mutable struct Response
    survey::SurveyID
    id::ResponseID
    page::Integer
    answers::Dict{Symbol, Answer}
    started::DateTime
    completed::Union{DateTime, Nothing}
end

Base.getindex(r::Response, id::Symbol) = r.answers[id]

# ---------------------
# Handling responses
# ---------------------

# Response templates

Answer(::Question{<:FormField{T}}) where {T} = Answer{T}(missing, nothing)

Response(s::Survey, id::ResponseID=rand(ResponseID)) =
    Response(s.id, id, 1,
             Dict(q.id => Answer(q) for q in
                      Iterators.flatten([s[i].questions for i in 1:length(s)])),
             now(), nothing)

function Response(s::Survey, oldids::Vector{ResponseID})
    newid = rand(ResponseID)
    while newid in oldids
        newid = rand(ResponseID)
    end
    Response(s, newid)
end

interpret(::FormField{<:AbstractString}, value::AbstractString) = value
interpret(::FormField{Integer}, value::AbstractString) = parse(Int64, value)
default_validators(::FormField{Integer}) = function(unparseable::String)
    "Integer required. \"$unparseable\" could not be parsed as an integer."
end
interpret(::FormField{Number}, value::AbstractString) =
    something(tryparse(Int64, value), parse(Float64, value))
default_validators(::FormField{Number}) = function(unparseable::String)
    "Number required. \"$unparseable\" could not be parsed as a number."
end
interpret(::FormField{T}, value::AbstractString) where {T} = parse(T, value)

# Response interpretation

function Answer(q::Question{<:FormField{T}}, value::Vector{String}) where {T}
    try
        processedvalue = interpret(q.field, ∘(identity, reverse(q.postprocessors)...)(value))
        error = nothing
        for validator in q.validators
            if applicable(validator, processedvalue)
                error = validator(processedvalue)
            end
            isnothing(error) || break
        end
        Answer{T}(processedvalue, error)
    catch e
        construction_error = nothing
        try
            for validator in q.validators
                if hasmethod(validator, Tuple{String})
                    construction_error = validator(last(value))
                elseif hasmethod(validator, Tuple{Vector{String}})
                    construction_error = validator(value)
                end
                isnothing(construction_error) || break
            end
        catch _
        end
        if isnothing(construction_error)
            @warn "Answer construction failure" exception = (e, catch_backtrace())
            construction_error = first(split(sprint(showerror, e), '\n'))
        end
        Answer{T}(missing, construction_error)
    end
end

function Answer(q::Question{<:FormField{T}}, ::Missing) where {T}
    if nonempty in q.validators
        Answer{T}(missing, nonempty(""))
    else
        Answer{T}(missing, nothing)
    end
end

# Response updating

function update!(r::Response, s::Survey, datum::Pair{Symbol, <:Any})
    id, value = datum
    if haskey(r.answers, id) && haskey(s.questions, id)
        r.answers[id] = Answer(s.questions[id], value)
    else
        @warn "$id not in response"
    end
end

function update!(r::Response, s::Survey, data::Dict{Symbol, <:Any})
    foreach(data) do datum
        update!(r, s, datum)
    end
end

clear!(r::Response, ids::Vector{Symbol}) =
    foreach(ids) do id
        r.answers[id] = Answer{typeof(r.answers[id])}()
    end

clear!(r::Response, q::Question) = clear!(r, [q.id])
clear!(r::Response, p::SurveyPart) = clear!(r, keys(p.questions))
clear!(r::Response, s::Survey) = clear!(r, keys(s.questions))
clear!(r::Response) = clear!(r, keys(r.answers))

# Response validity

isvalid(a::Answer) = isnothing(a.error)
isvalid(a::Answer, q::Question) =
    isvalid(a) && !(isempty(a) && nonempty in q.validators)
isvalid(r::Response, q::Question) = isvalid(r.answers[q.id], q)
isvalid(r::Response, p::SurveyPart) = all(isvalid.(Ref(r), p.questions))
isvalid(r::Response, s::Survey) =
    all(isvalid.(Ref(r), Iterators.flatten([s[i].questions for i in 1:length(s)])))

# ---------------------
# General htmlrenderer
# ---------------------

html_content(::FormField, value) = ""
html_element(::FormField) = "?"
html_attrs(::FormField, value) = []
html_voidelem(::FormField) = false
html_postprocess(::FormField, ::Symbol) = identity

elem(e::AbstractString, content::AbstractString="", attrs::Pair{Symbol,<:Any}...) =
    Html.normal_element(content, e, [], attrs...)
elem(e::AbstractString, attrs::Pair{Symbol,<:Any}...) = elem(e, "", attrs...)

velem(e::AbstractString, attrs::Pair{Symbol,<:Any}...) =
    Html.void_element(e, [], Vector{Pair{Symbol,Any}}(collect(attrs)))

const html_escape_characters =
    Dict('"' => "&quot;",
         '&' => "&amp;",
         '<' => "&lt;",
         '>' => "&gt;")
html_escape(s::String) = replace(s, r"\"|&|<|>" => c -> html_escape_characters[c[1]])
html_escape(::Missing) = ""

function htmlrender(field::FormField, value::Any, id, mandatory, invalid)
    element = html_element(field)
    attrs = vcat(html_attrs(field, value),
                 [:id => string("qn-", id),
                  :name => id,
                  Symbol("aria-invalid") => invalid,
                  :required => mandatory && !isa(field, Checkbox)])
    if html_voidelem(field)
        velem(element, attrs...)
    else
        content = html_content(field, value)
        elem(element, if ismissing(content) "" else string(content) end,
             attrs...)
    end |> html_postprocess(field, id)
end

# ---------------------
# Form fields & html rendering
# ---------------------

# <input>

struct FormInput{T} <: FormField{T}
    FormInput{T}() where {T} =
        if T <: Union{Bool, String, Integer, Number, Date}
            new()
        else
            TypeError(:FormInput, Union{Bool, String, Integer, Number, Date}, T)
        end
end

html_element(::FormInput) = "input"
html_voidelem(::FormInput) = true
html_attrs(::FormInput, value) =
    [:value => if ismissing(value) false else html_escape(string(value)) end]

# <input type="checkbox">

interpret(::FormInput{Bool}, value::AbstractString) = value == "yes"
html_attrs(::FormInput{Bool}, value::Union{Bool, Missing}) =
    [:type => "checkbox", :value => "yes", :checked => !ismissing(value) && value === true]
html_postprocess(::FormInput{Bool}, id::Symbol) =
    s -> string(input(type="hidden", name=id, value="no"), s)

# <input type="date|number|text">

function html_attrs(::FormInput{T}, value) where {T <: Union{Date, Number, Integer, String}}
    type = Dict(Date => "date",
                Number => "number",
                Integer => "number",
                String => "text")[T]
    [:type => type,
     :value => if ismissing(value) false else html_escape(string(value)) end]
end

const Checkbox = FormInput{Bool}
const TextInput = FormInput{String}
const DateInput = FormInput{Date}
const NumberInput = FormInput{Number}
const IntegerInput = FormInput{Integer}

default_validators(::TextInput) = Function[wordlimit(100), charlimit(500)]

# <textarea>

struct TextArea <: FormField{String} end

default_validators(::TextArea) = Function[wordlimit(500), charlimit(2500)]

html_element(::TextArea) = "textarea"
html_content(::TextArea, value) = html_escape(value)

# <select>

struct Options
    options::Vector{Pair{String, String}}
end
Options(vals::Vector{String}) = Options(map(v -> v => v, vals))

struct OptGroup
    group::String
    options::Options
end
OptGroup(gopt::Pair{String, Vector}) = OptGroup(gopt.first, Options(gopt.second))

struct Dropdown{O <: Union{Options, Vector{OptGroup}}} <: FormField{String}
    options::O
end
Dropdown(opts::Union{Vector{String}, Vector{Pair{String, String}}}) =
    Dropdown(Options(opts))
Dropdown(gopts::Vector{Pair{String, Vector}}) =
    Dropdown([OptGroup(gopt) for gopt in gopts])

html_element(::Dropdown) = "select"
function html_content(d::Dropdown{Options}, value)
    string(option("Select one", value="", selected=ismissing(value),
                  disabled=true, hidden=true), '\n',
           map(opt -> option(opt.second, value=opt.first,
                             selected=(!ismissing(value) && value==opt.first)) * '\n',
               d.options.options)...)
end

function html_content(d::Dropdown{Vector{OptGroup}}, value)
    string(option("Select one", value="", selected=ismissing(value),
                  disabled=true, hidden=true),
           map(d.options) do optgroup
               elem("optgroup",
                    string(map(opt -> option(opt.second, value=opt.first,
                                             selected=value==opt.first),
                               d.options)...),
                    :label => optgroup.group)
           end...)
end

# Many <input type="radio">

struct RadioSelect <: FormField{String}
    options::Options
    other::Bool
    # Without an inner constructor an (::Any, ::Any) constructor
    # is created for /some/ reason ... why!?
    RadioSelect(options::Options, other::Bool) = new(options, other)
end

# Many <input type="checkbox">

struct MultiSelect <: FormField{Vector{String}}
    options::Options
    other::Bool
    # Without an inner constructor an (::Any, ::Any) constructor
    # is created for /some/ reason ... why!?
    MultiSelect(options::Options, other::Bool) = new(options, other)
end

# Many <input type="radio|checkbox">

function (F::Type{<:Union{RadioSelect,MultiSelect}})(opts::Vector{T}) where {T}
    if T == Any
        try
            if Pair{String,String} in typeof.(opts)
                opts = map(opts) do o
                    if o isa String
                        o => o
                    else
                        o
                    end
                end |> Vector{Union{Pair{String,String},Symbol}}
            else
                opts = Vector{Union{String,Symbol}}(opts)
            end
        catch e
            @error "Could not coerce Vector{$T} to a Vector{Union{String, Pair{String, String}, Symbol}}"
            rethrow(e)
        end
    end
    nosymbol = filter(o -> o isa String || o isa Pair{String,String}, opts)
    F(Options(Vector{typeof(first(nosymbol))}(nosymbol)), :other in opts)
end

interpret(::FormField{Vector{String}}, value::Vector{<:AbstractString}) =
    value

default_postprocessors(::MultiSelect) =
    Function[s -> filter(!isempty, strip.(s))]
default_postprocessors(::RadioSelect) =
    Function[s -> filter(!isempty, strip.(s)), last]

# Render many <input type="radio|checkbox">

function htmlrender(q::Union{<:Question{RadioSelect}, Question{MultiSelect}},
                    value, invalid::Union{Bool, String})
    if !ismissing(value)
        value = vcat(value)
    end
    type = if q isa Question{RadioSelect} "radio" else "checkbox" end
    string(elem("legend", q.prompt,
            Symbol("data-mandatory") =>
                if nonempty in q.validators "true" else false end),
        '\n',
        join(map(q.field.options.options |> enumerate) do (i, opt)
                id = string("qn-", q.id, "-", opt.second)
                elem("label",
                    elem("input", :type => type, :id => id,
                        :name => string(q.id, "[]"), :value => html_escape(opt.second),
                        :checked => (!ismissing(value) && opt.second ∈ value),
                        if type == "radio" && q.field.other
                            [:oninput => "document.getElementById('$(string("qn-", q.id, "--other-input"))').value = ''"]
                        else [] end...) *
                    opt.first,
                    :for => id)
            end, '\n'),
        if q.field.other
            othervals = if !ismissing(value)
                filter(v -> v ∉ getfield.(q.field.options.options, :second), value)
            else [] end
            '\n' *
            elem("label",
                string(
                    elem("input", :type => type, :name => if type == "radio"
                            string(q.id, "[]")
                        else "" end,
                        :id => string("qn-", q.id, "--other"), :value => "",
                        :checked => length(othervals) > 0,
                         if type == "checkbox"
                            [:oninput => "if (!this.checked) { document.getElementById('$(string("qn-", q.id, "--other-input"))').value = '' }"]
                         else [] end...),
                    elem("input", :type => "text",
                        :id => string("qn-", q.id, "--other-input"),
                        :class => "other", :placeholder => "Other",
                        :name => string(q.id, "[]"), :value => html_escape(join(othervals, ", ")),
                        :oninput => "document.getElementById('$(string("qn-", q.id, "--other"))').checked = this.value.length > 0"
                    )),
                :for => string("qn-", q.id, "--other"))
        else "" end) |> fieldset
end

# <input type="range">

struct RangeSelect <: FormField{Number}
    values::StepRange{<:Number, <:Number}
end

RangeSelect(r::UnitRange{<:Number}) = RangeSelect(StepRange(r.start, 1, r.stop))

html_element(::RangeSelect) = "input"
html_voidelem(::RangeSelect) = true
html_attrs(r::RangeSelect, value) =
    [:type => "range",
     :min => r.values.start, :max => r.values.stop,
     :step => r.values.step, :style => "--stops: $(length(r.values)-1); width: calc(100% - 3em)",
     :oninput => "results$(hash(r)).value = this.value",
     :value => if ismissing(value) false else html_escape(string(value)) end]
html_postprocess(r::RangeSelect, id::Symbol) =
    s -> string(s, '\n', output(name="results$(hash(r))"), '\n',
                script("""let r = document.getElementById("qn-$id")
let o = document.getElementsByName("results$(hash(r))")[0]
r.style.width = "calc(100% - 3em)"
o.value = r.value"""))

# ---------------------
# Survey Renderer (HTML)
# ---------------------

function htmlrender(q::Question, value, invalid::Union{Bool, String})
    mandatory = nonempty in q.validators
    rfield = htmlrender(q.field, value, q.id, mandatory, invalid)
    elem("label",
         if q.field isa FormInput{Bool}
             rfield * span(q.prompt)
         else
             span(q.prompt) * rfield
         end,
         :for => "qn-"*string(q.id),
         Symbol("data-mandatory") => string(mandatory)) |>
             if q.field isa Checkbox; fieldset else identity end
end

function htmlrender(q::Question, a::Answer)
    if isvalid(a)
        htmlrender(q, a.value, false)
    else
        string(elem("small", a.error, :class => "formerror"),
               htmlrender(q, a.value, string(!isnothing(a.error))))
    end
end

show(io::IO, ::MIME"text/html", q::Question) = print(io, htmlrender(q, missing), '\n')
function show(io::IO, ::MIME"text/html", (q, a)::Pair{<:Question, <:Answer})
    print(io, htmlrender(q, a), '\n')
end

function Base.show(io::IO, m::MIME"text/html", part::SurveyPart)
    foreach(part.questions) do q
        show(io, m, q)
        q === last(part.questions) || print(io, br(), '\n')
    end
end

function show(io::IO, m::MIME"text/html", (part, response)::Pair{SurveyPart, Response})
    foreach(part.questions) do q
        show(io, m, q => response[q.id])
        q === last(part.questions) || print(io, br(), '\n')
    end
end

# ---------------------
# Survey Renderer (Plain)
# ---------------------

function show(io::IO, ::MIME"text/plain", q::Question{T}) where {T}
    printstyled(io, "  Q{", bold=true, color=:blue)
    printstyled(io, string(T), color=:blue)
    printstyled(io, "}: ", bold=true, color=:blue)
    print(io, q.prompt)
    if nonempty in q.validators
        printstyled(io, "*", color=:red)
    end
end

function show(io::IO, ::MIME"text/plain", a::Answer)
    printstyled(io, "  A: ", bold=true, color=:green)
    if ismissing(a.value)
        printstyled(io, "missing\n", color=:light_black)
    else
        print(io, a.value)
    end
    if !isnothing(a.error)
        printstyled(io, "     ! ", a.error, color=:yellow)
    end
end

function show(io::IO, m::MIME"text/plain", (q, a)::Pair{<:Question, <:Answer})
    show(io, m, q)
    print(io, '\n')
    show(io, m, a)
end
show(io::IO, qa::Pair{<:Question, <:Answer}) = show(io, MIME("text/plain"), qa)

function show(io::IO, m::MIME"text/plain", part::SurveyPart)
    printstyled(io, "  -- ", if isnothing(part.label)
                    "Unlabeled part"
                else part.label end, " --\n", color=:yellow)
    if !isnothing(part.description)
        printstyled(io, "  ", part.description, '\n', color=:yellow)
    end
    for q in part.questions
        show(io, m, q)
        q === last(part.questions) || print(io, "\n")
    end
end

function show(io::IO, m::MIME"text/plain", (part, response)::Pair{SurveyPart, Response})
    printstyled(io, "  -- ", if isnothing(part.label)
                    "Unlabeled part"
                else part.label end, " --\n", color=:yellow)
    foreach(part.questions) do q
        show(io, m, q => response[q.id])
        last(part.questions) === q || print(io, '\n')
    end
end
show(io::IO, pr::Pair{SurveyPart, Response}) = show(io, MIME("text/plain"), pr)

function show(io::IO, m::MIME"text/plain", s::Survey)
    printstyled(io, "  ", s.name, '\n', color=:magenta)
    parts = [s[p] for p in 1:length(s)]
    for part in parts
        show(io, m, part)
        part === last(parts) || print(io, '\n')
    end
end

function show(io::IO, m::MIME"text/plain", (s, response)::Pair{Survey, Response})
    printstyled(io, "  ", s.name, "\n\n", color=:magenta)
    parts = [s[p] for p in 1:length(s)]
    for part in parts
        show(io, m, part => response)
        part === last(parts) || print(io, "\n\n")
    end
end
show(io::IO, sr::Pair{Survey, Response}) = show(io, MIME("text/plain"), sr)

function show(io::IO, m::MIME"text/plain", r::Response)
    printstyled(io, "Response $(r.id) to survey $(r.survey)\n", color=:magenta)
    print(io, "  Started:   ")
    printstyled(string(r.started), '\n', color=:green)
    print(io, "  Completed: ")
    if isnothing(r.completed)
        printstyled(io, "no", '\n', color=:light_black)
        print(io, "  Page:      ")
        printstyled(io, r.page, '\n', color=:blue)
    else
        printstyled(io, string(r.completed), '\n', color=:blue)
    end
    print("Answers: ")
    show(io, m, r.answers)
end

# ---------------------
# Survey Renderer (Org)
# ---------------------

function show(io::IO, ::MIME"text/org", q::Question{T}) where {T}
    printstyled(io, "** ", q.prompt, bold=true, color=:blue)
    printstyled(io, " :", string(T), ":", color=:light_black)
    if nonempty in q.validators
        printstyled(io, "Mandatory:", color=:light_black)
    end
    print(io, '\n')
    if hasfield(T, :options)
        for option in q.field.options.options
            println(io, "- [ ] ", first(option))
        end
        if hasfield(T, :other) && q.field.other
            println(io, "- [ ] *other*")
        end
    end
end

function show(io::IO, m::MIME"text/org", part::SurveyPart)
    printstyled(io, "* ", if isnothing(part.label)
                    "Unlabeled part"
                else part.label end, "\n", color=:yellow)
    if !isnothing(part.description)
        printstyled(io, part.description, '\n', color=:yellow)
    end
    print(io, '\n')
    for q in part.questions
        show(io, m, q)
        q === last(part.questions) || print(io, "\n")
    end
end

function show(io::IO, m::MIME"text/org", s::Survey)
    printstyled(io, "#+title: ", s.name, "\n", color=:magenta)
    if !isnothing(s.description)
        print(io, '\n', s.description, '\n')
    end
    print(io, '\n')
    parts = [s[p] for p in 1:length(s)]
    for part in parts
        show(io, m, part)
        part === last(parts) || print(io, '\n')
    end
end

end
