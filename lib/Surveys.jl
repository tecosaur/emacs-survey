module Surveys

using Dates
using Genie.Renderers.Html
import Base: show, isvalid, isempty

export Survey, SurveyPart, Question,
    Answer, Response, update!, clear!, nonempty,
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

lastsoftfail(v::Vector) = if length(v) > 0
    last(v)
else
    ""
end

defaultpostprocessors(::FormField) = Function[last, strip]

function Question(id::Symbol, prompt::AbstractString, field::FormField;
                  validators::Union{Function, Vector{<:Function}}=Function[],
                  postprocessors::Union{Function, Vector{<:Function}}=defaultpostprocessors(field),
                  mandatory::Bool=true)
    fullvalidators = if mandatory
        vcat(nonempty, validators)
    else
        vcat(validators)
    end
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

function (F::Type{<:FormField})(id::Symbol, prompt::AbstractString, args...;
                                validators::Union{Function, Vector{<:Function}}=Function[],
                                mandatory::Bool=true,
                                kwargs...)
    Question(id, prompt, F(args...; kwargs...); validators, mandatory)
end
function (F::Type{<:FormField})(prompt::AbstractString, args...; kwargs...)
    F(prompttoid(prompt), prompt, args...; kwargs...)
end

# Survey Part

struct SurveyPart
    label::Union{AbstractString, Nothing}
    questions::Vector{Question}
end

Base.getindex(p::SurveyPart, id::Symbol) = findfirst(q -> q.id == id, p.questions)
Base.getindex(p::SurveyPart, index::Integer) = p.questions[index]

Base.length(p::SurveyPart) = length(p.questions)

SurveyPart(label::Union{AbstractString, Nothing}, questions::Question...) =
    SurveyPart(label, questions |> collect)
SurveyPart(questions::Question...) = SurveyPart(nothing, questions |> collect)

# Survey

const SurveyID = UInt32

struct Survey
    id::SurveyID
    name::AbstractString
    description::Union{AbstractString, Nothing}
    parts::Vector{Pair{Union{AbstractString, Nothing}, Vector{Symbol}}}
    questions::Dict{Symbol, Question}
end

Base.getindex(s::Survey, id::Symbol) = s.questions[id]
Base.getindex(s::Survey, part::Integer) =
    SurveyPart(s.parts[part].first,
               getindex.(Ref(s.questions), s.parts[part].second))

Base.length(s::Survey) = length(s.parts)

function Survey(name::AbstractString,
                description::Union{AbstractString, Nothing},
                parts::Vector{<:Pair{<:Union{<:AbstractString, Nothing}, <:Vector{Symbol}}},
                questions::Dict{Symbol, Question})
    # Create an id that only depends on:
    # 1. Question IDs
    # 2. Question Prompts
    # 3. Question field types
    # Hopefully memhashing a Tuple of Symbols and Strings is somewhat stable,
    # I checked this on Julia 1.3 and 1.6 and it looked alright.
    id = xor(map(values(questions)) do q
                 hash((q.id, q.prompt, string(typeof(q.field))))
             end...) |>
                 h -> xor(reinterpret(SurveyID, [h])...)
    Survey(id, name, description, parts, questions)
end

Survey(name::AbstractString,
       description::Union{AbstractString, Nothing},
       parts::SurveyPart...) =
           Survey(name, description,
                  map(p -> p.label => getfield.(p.questions, :id), parts) |> collect,
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
interpret(::FormField{Number}, value::AbstractString) =
    something(tryparse(Int64, value), parse(Float64, value))
interpret(::FormField{T}, value::AbstractString) where {T} = parse(T, value)

# Response interpretation

function Answer(q::Question{<:FormField{T}}, value::Union{String, Vector{String}}) where {T}
    try
        processedvalue = interpret(q.field, ∘(identity, reverse(q.postprocessors)...)(value))
        error = nothing
        for validator in q.validators
            error = validator(processedvalue)
            isnothing(error) || break
        end
        Answer{T}(processedvalue, error)
    catch e
        @warn "Answer construction failure" exception=(e, catch_backtrace())
        Answer{T}(missing, first(split(sprint(showerror, e), '\n')))
    end
end

function Answer(q::Question{<:FormField{T}}, ::Missing) where {T}
    if nonempty in q.validators
        Answer{T}(missing, "Must be answered")
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

htmlcontent(::FormField, value) = ""
htmlelement(::FormField) = "?"
htmlattrs(::FormField, value) = []
htmlvoidelem(::FormField) = false
htmlpostprocess(::FormField, ::Symbol) = identity

elem(e::AbstractString, content::AbstractString="", attrs::Pair{Symbol,<:Any}...) =
    Html.normal_element(content, e, [], attrs...)
elem(e::AbstractString, attrs::Pair{Symbol,<:Any}...) = elem(e, "", attrs...)

velem(e::AbstractString, attrs::Pair{Symbol,<:Any}...) =
    Html.void_element(e, [], Vector{Pair{Symbol,Any}}(collect(attrs)))

function htmlrender(field::FormField, value::Any, id, mandatory, invalid)
    element = htmlelement(field)
    attrs = vcat(htmlattrs(field, value),
                 [:id => string("qn-", id),
                  :name => id,
                  Symbol("aria-invalid") => invalid,
                  :required => mandatory && !isa(field, Checkbox)])
    if htmlvoidelem(field)
        velem(element, attrs...)
    else
        content = htmlcontent(field, value)
        elem(element, if ismissing(content) "" else string(content) end,
             attrs...)
    end |> htmlpostprocess(field, id)
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

htmlelement(::FormInput) = "input"
htmlvoidelem(::FormInput) = true
htmlattrs(::FormInput, value) =
    [:value => if ismissing(value) false else string(value) end]

# <input type="checkbox">

interpret(::FormInput{Bool}, value::AbstractString) = value == "yes"
htmlattrs(::FormInput{Bool}, value::Union{Bool, Missing}) =
    [:type => "checkbox", :value => "yes", :checked => !ismissing(value) && value === true]
htmlpostprocess(::FormInput{Bool}, id::Symbol) =
    s -> string(input(type="hidden", name=id, value="no"), s)

# <input type="date|number|text">

function htmlattrs(::FormInput{T}, value) where {T <: Union{Date, Number, Integer, String}}
    type = Dict(Date => "date",
                Number => "number",
                Integer => "number",
                String => "text")[T]
    [:type => type,
     :value => if ismissing(value) false else string(value) end]
end

const Checkbox = FormInput{Bool}
const TextInput = FormInput{String}
const DateInput = FormInput{Date}
const NumberInput = FormInput{Number}
const IntegerInput = FormInput{Integer}

# <textarea>

struct TextArea <: FormField{String} end
htmlelement(::TextArea) = "textarea"
htmlcontent(::TextArea, value) = value

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

htmlelement(::Dropdown) = "select"
function htmlcontent(d::Dropdown{Options}, value)
    string(option("Select one", value="", selected=ismissing(value),
                  disabled=true, hidden=true), '\n',
           map(opt -> option(opt.second, value=opt.first,
                             selected=(!ismissing(value) && value==opt.first)) * '\n',
               d.options.options)...)
end

function htmlcontent(d::Dropdown{Vector{OptGroup}}, value)
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

function (F::Type{<:Union{RadioSelect, MultiSelect}})(opts::Vector)
    nosymbol = filter(o -> o isa String || o isa Pair{String, String}, opts)
    F(Options(Vector{typeof(first(nosymbol))}(nosymbol)), :other in opts)
end

interpret(::FormField{Vector{String}}, value::Vector{<:AbstractString}) =
    value

defaultpostprocessors(::MultiSelect) = Function[s -> filter(!isempty, strip.(s))]
defaultpostprocessors(::RadioSelect) = Function[s -> filter(!isempty, strip.(s)), last]

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
                    id = string("qn-", q.id, "-", opt.first)
                    elem("label",
                         elem("input", :type => type, :id => id,
                              :name => string(q.id, "[]"), :value => opt.first,
                              :checked => (!ismissing(value) && opt.first ∈ value),
                              if q.field.other
                                  [:oninput => "document.getElementById('$(string("qn-", q.id, "--other-input"))').value = ''"]
                              else [] end...) *
                                  opt.second,
                         :for => id)
                end, '\n'),
           if q.field.other
               othervals = if !ismissing(value)
                   filter(v -> v ∉ getfield.(q.field.options.options, :first), value)
               else
                   []
               end
               '\n' *
                   elem("label",
                        string(
                            elem("input", :type => type, :name => if type == "radio" string(q.id, "[]") else "" end,
                                 :id => string("qn-", q.id, "--other"), :value => "",
                                 :checked => length(othervals) > 0),
                            elem("input", :type => "text",
                                 :id => string("qn-", q.id, "--other-input"),
                                 :class => "other", :placeholder => "Other",
                                 :name => string(q.id, "[]"), :value => join(othervals, ", "),
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

htmlelement(::RangeSelect) = "input"
htmlvoidelem(::RangeSelect) = true
htmlattrs(r::RangeSelect, value) =
    [:type => "range",
     :min => r.values.start, :max => r.values.stop,
     :step => r.values.step, :style => "--stops: $(length(r.values)-1); width: calc(100% - 3em)",
     :oninput => "results$(hash(r)).value = this.value",
     :value => if ismissing(value) false else string(value) end]
htmlpostprocess(r::RangeSelect, id::Symbol) =
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
function show(io::IO, ::MIME"text/html", qa::Pair{<:Question, <:Answer})
    q, a = qa
    print(io, htmlrender(q, a), '\n')
end

function Base.show(io::IO, m::MIME"text/html", part::SurveyPart)
    foreach(part.questions) do q
        show(io, m, q)
        q === last(part.questions) || print(io, br(), '\n')
    end
end

function show(io::IO, m::MIME"text/html", pr::Pair{SurveyPart, Response})
    part, response = pr
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

function show(io::IO, m::MIME"text/plain", qa::Pair{<:Question, <:Answer})
    q, a = qa
    show(io, m, q)
    print(io, '\n')
    show(io, m, a)
end
show(io::IO, qa::Pair{<:Question, <:Answer}) = show(io, MIME("text/plain"), qa)

function show(io::IO, m::MIME"text/plain", part::SurveyPart)
    for q in part.questions
        show(io, m, q)
        q === last(part.questions) || print(io, "\n")
    end
end

function show(io::IO, m::MIME"text/plain", pr::Pair{SurveyPart, Response})
    part, response = pr
    foreach(part.questions) do q
        show(io, m, q => response[q.id])
        last(part.questions) === q || print(io, "\n\n")
    end
end
show(io::IO, pr::Pair{SurveyPart, Response}) = show(io, MIME("text/plain"), pr)

function show(io::IO, m::MIME"text/plain", s::Survey)
    printstyled(io, "  ", s.name, '\n', color=:magenta)
    parts = [s[p] for p in 1:length(s)]
    for part in parts
        show(io, m, part)
        part === last(parts) || printstyled(io, "\n  ---\n", color=:yellow)
    end
end

function show(io::IO, m::MIME"text/plain", sr::Pair{Survey, Response})
    s, response = sr
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

end
