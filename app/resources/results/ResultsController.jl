module ResultsController

using Genie, Genie.Renderer, Genie.Renderers.Html, HTTP, JSON3, StatsBase

using Results, Surveys

function resultsindex(survey::Union{SurveyID, Nothing}=nothing)
    if isnothing(survey)
        html(:results, :listsurveys, layout=:base;
             surveys, questions, responseids)
    else
        html(:results, :survey, layout=:base;
             name=surveys()[survey], id=survey,
             questions, sresults=results(survey),
             countmap)
    end
end

function resultsfile(survey::SurveyID, format::AbstractString)
    @assert survey in keys(surveys())
    if format == "txt"
        WebRenderable(sprint(show, results(survey)), :text) |>
            Genie.Renderer.respond
    elseif format in ("csv", "tsv", "json")
        WebRenderable(results(survey, format=Symbol(format)),
                      if format == "json"; :json else :text end) |>
                          Genie.Renderer.respond
    elseif format == "db" || format == "sqlite"
        HTTP.Response(200, ["Content-Type" => "application/octet-stream"],
                      body = results(survey, format=:sqlite))
    else
        error("format $format not recognised")
    end
end

function listsurveys()
    WebRenderable(sprint(JSON3.pretty,
                         [Dict(:id => id,
                               :name => name)
                          for (id, name) in Results.surveys()]),
                  :json) |> Genie.Renderer.respond
end

end
