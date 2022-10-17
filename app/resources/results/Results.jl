module Results

using SearchLight
using ..Main.UserApp.Surveys, Dates
using DataFrames, CSV, JSON3, SQLite

export surveys, questions, responseids, results,
    register!, deregister!, save!, clear!

const SURVEYS = Dict{SurveyID, String}()
const QUESTIONS = Dict{SurveyID, Dict{Symbol, Dict{Symbol, Any}}}()
const RESPONSES = Dict{SurveyID, Dict{ResponseID, Union{Response, Nothing}}}()

# ---------------------
# Reading
# ---------------------

function surveys(;cache::Bool=true)
    if !cache || isempty(SURVEYS)
        foreach(SearchLight.query("SELECT * from surveys") |> eachrow) do s
            SURVEYS[SurveyID(s.id)] = s.name
        end
    end
    SURVEYS
end
surveys(id::SurveyID; cache::Bool=true) = surveys(;cache)[id]

function questions(survey::SurveyID; cache::Bool=true)
    if !haskey(QUESTIONS, survey) || !cache
        qns = SearchLight.query("SELECT id, type, prompt, input FROM questions WHERE survey=$survey")
        qns.id = Symbol.(qns.id)
        qns.type = Vector{Type}(@. Surveys.eval(Meta.parse(qns.type)))
        qns.input = Vector{Type}(@. Surveys.eval(Meta.parse.(qns.input)))
        QUESTIONS[survey] = Dict(q.id => Dict(:type => q.type, :prompt => q.prompt, :input => q.input) for q in eachrow(qns))
    end
    QUESTIONS[survey]
end

function responseids(survey::SurveyID; cache::Bool=true)
    if !haskey(RESPONSES, survey) || !cache
        ids = SearchLight.query("SELECT DISTINCT response FROM results WHERE survey=$survey") |>
            r -> ResponseID.(r.response)
        if !haskey(RESPONSES, survey)
            RESPONSES[survey] =
                Dict{ResponseID, Union{Response, Nothing}}(id => nothing for id in ids)
        else
            foreach(ids) do id
                if !haskey(RESPONSES[survey], id)
                    RESPONSES[survey][id] = nothing
                end
            end
        end
    end
    Vector{ResponseID}(keys(RESPONSES[survey]) |> collect)
end

function responseids(survey::SurveyID, type::Symbol; cache::Bool=true)
    if type == :all
        responseids(surveyid; cache)
    elseif type == :complete
        SearchLight.query("SELECT id FROM responses WHERE survey=$survey AND completed IS NOT NULL") |>
            r -> ResponseID.(r.id)
    elseif type == :incomplete
        SearchLight.query("SELECT id FROM responses WHERE survey=$survey AND completed IS NULL") |>
            r -> ResponseID.(r.id)
    end
end

function response(survey::SurveyID, response::ResponseID; cache::Bool=true)
    if cache && response ∉ responseids(survey; cache)
        responseids(survey, cache=false)
    end
    @assert response in responseids(survey; cache)
    if RESPONSES[survey][response] === nothing
        responsedata = SearchLight.query("SELECT question, value FROM results WHERE survey=$survey AND response=$response")
        answers = Dict(map(eachrow(responsedata)) do ans
                           qid = Symbol(ans.question)
                           anstype = questions(survey)[qid][:type]
                           value = eval(Meta.parse(ans.value))
                           qid => Answer{anstype}(value, nothing)
                       end)
        metadata = SearchLight.query("SELECT started, completed, page FROM responses WHERE survey=$survey AND id=$response")
        started = parse(DateTime, metadata.started[1])
        completed = if !ismissing(metadata.completed[1])
            parse(DateTime, metadata.completed[1]) end
        RESPONSES[survey][response] =
            Response(survey, response, metadata.page[1], answers,
                     started, completed)
    end
    RESPONSES[survey][response]
end

function results(survey::SurveyID; cache::Bool=true, format::Symbol=:DataFrame)
    resids = responseids(survey; cache)
    results(survey, resids; cache, format)
end

function results(survey::SurveyID, resids::Vector{ResponseID};
                 cache::Bool=true, format::Symbol=:DataFrame)
    res = response.(survey, resids; cache)
    qids = keys(questions(survey; cache))
    data = Dict(q => map(r -> r[q].value, res) for q in qids)
    @info "" DataFrame(data)
    DataFrame(data) |> if format == :DataFrame
        identity
    elseif format == :csv
        df -> sprint(CSV.write, df)
    elseif format == :tsv
        df -> sprint((io, df) -> CSV.write(io, df, delim='\t'), df)
    elseif format == :json
        df -> sprint(JSON3.pretty,
                     [Dict([string(col) => df[i, col]
                            for col in names(df)])
                      for i in 1:size(df, 1)])
    elseif format == :sqlite
        df -> (mktemp() do path, io
                   df_plain = DataFrame()
                   for (name, col) in zip(names(df), eachcol(df))
                       df_plain[!, name] = if eltype(col) <: Union{Int, String}
                           col else sprint.(print, col) end
                   end
                   SQLite.load!(df_plain, SQLite.DB(path), "results")
                   read(path)
               end)
    end
end

questions(s::Survey; cache::Bool=true) = questions(s.id; cache)
responseids(s::Survey; cache::Bool=true) = responseids(s.id; cache)
response(s::Survey; cache::Bool=true) = response(s.id; cache)
results(s::Survey; cache::Bool=true) = results(s.id; cache)

# ---------------------
# Writing
# ---------------------

function register!(survey::Survey)
    qvaltype(::Question{<:Surveys.FormField{T}}) where {T} = T
    if survey.id ∉ keys(surveys())
        name = if isnothing(survey.name) "NULL" else
            SearchLight.escape_value(survey.name) end
        srepr = repr(survey, context = :module => Surveys) |> SearchLight.escape_value
        SearchLight.query("INSERT INTO surveys (id, name, repr) VALUES ($(survey.id), $name, $srepr)")
        for (qid, q) in survey.questions
            qid_s = string(qid) |> SearchLight.escape_value
            prompt = q.prompt |> SearchLight.escape_value
            type = string(qvaltype(q)) |> SearchLight.escape_value
            field = sprint(print, typeof(q.field), context = :module => Surveys) |> SearchLight.escape_value
            field = sprint(print, typeof(q.field), context = :module => Surveys) |> SearchLight.escape_value
            SearchLight.query("INSERT INTO questions (survey, id, type, prompt, input) VALUES ($(survey.id), $qid_s, $type, $prompt, $field)")
        end
        SURVEYS[survey.id] = survey.name
    end
end

function register!(response::Response, exip::Integer=0; cache::Bool=true)
    if response.survey ∉ keys(surveys(;cache))
        register!(response.survey)
    end
    @assert response.id ∉ responseids(response.survey; cache)
    SearchLight.query(
        "INSERT INTO responses (survey, id, exip, started, page) \
         VALUES ($(response.survey), $(response.id), $exip, \
                 '$(string(response.started))', $(response.page))")
    for (qid, ans) in response.answers
        qid_s = SearchLight.escape_value(string(qid))
        value_s = SearchLight.escape_value(repr(ans.value))
        SearchLight.query("INSERT INTO results (survey, response, question, value) VALUES ($(response.survey), $(response.id), $qid_s, $value_s)")
    end
    RESPONSES[response.survey][response.id] = nothing
end

function deregister!(response::Response; cache::Bool=true)
    @assert response.id ∈ responseids(response.survey; cache)
    SearchLight.query("DELETE FROM responses WHERE survey = $(response.survey) AND id = $(response.id)")
    SearchLight.query("DELETE FROM results WHERE survey = $(response.survey) AND response = $(response.id)")
    delete!(RESPONSES[response.survey], response.id)
end

function save!(response::Response, questionsids::Vector{Symbol}=keys(response.answers); cache::Bool=true)
    @assert response.id ∈ responseids(response.survey; cache)
    if !isnothing(response.completed)
        SearchLight.query("UPDATE responses SET completed = '$(string(response.completed))' WHERE survey = $(response.survey) AND id = $(response.id)")
    end
    SearchLight.query("UPDATE responses SET page = $(response.page) WHERE survey = $(response.survey) AND id = $(response.id)")
    for qid in questionsids
        qid_s = SearchLight.escape_value(string(qid))
        ans = response.answers[qid]
        value_s = SearchLight.escape_value(repr(ans.value))
        SearchLight.query("UPDATE results SET value = $value_s WHERE survey = $(response.survey) AND response = $(response.id) AND question = $qid_s")
    end
end

save!(response::Response, questions::Vector{Question}; cache::Bool=true) =
    save!(response, getfield.(questions, :id); cache)

save!(response::Response, question::Question; cache::Bool=true) =
    save!(response, [question.id]; cache)

save!(response::Response, part::SurveyPart; cache::Bool=true) =
    save!(response, part.questions; cache)

save!(response::Response, survey::Survey; cache::Bool=true) =
    save!(response, survey.questions; cache)

function clear!(survey::Survey)
    SearchLight.query("DELETE FROM survey WHERE id = $(survey.id)")
    SearchLight.query("DELETE FROM questions WHERE survey = $(survey.id)")
    SearchLight.query("DELETE FROM responses WHERE survey = $(survey.id)")
    SearchLight.query("DELETE FROM results WHERE survey = $(survey.id)")
end

function clear!(response::Response)
    SearchLight.query("DELETE FROM responses WHERE survey = $(response.survey), id = $(response.id)")
    SearchLight.query("DELETE FROM results WHERE survey = $(response.survey), response = $(response.id)")
end

end
