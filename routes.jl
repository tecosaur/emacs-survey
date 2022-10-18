using Genie.Router, Genie.Requests, Genie.Renderers.Html

using ..Main.UserApp.SurveysController
using ..Main.UserApp.ResultsController

route("/") do
    SurveysController.index()
end

route("/survey") do
    SurveysController.serve(getpayload())
end

route("/submit", method=POST) do
    SurveysController.submit(postpayload())
end

route("/submit-backpage", method=POST) do
    SurveysController.submit(postpayload(), backpage=true)
end

route("/results/?") do; ResultsController.resultsindex() end

route("/results/:survey#([A-Za-z0-9]+)",
      named = :surveyindex) do
    surveyid = tryparse(SurveysController.SurveyID, payload(:survey), base=10)
    ResultsController.resultsindex(surveyid)
end

route("/results/:surveyandformat#([A-Za-z0-9]+\\.[a-z0-9]+)") do
    @info "" payload(:surveyandformat)
    survey, format = match(r"([A-Za-z0-9]+)\.([a-z0-9]+)", payload(:surveyandformat)).captures
    surveyid = tryparse(SurveysController.SurveyID, survey, base=10)
    ResultsController.resultsfile(surveyid, format)
end

route("/results/:survey#([A-Za-z0-9]+)/:format#([a-z0-9]+)",
      named = :surveyresult) do
    surveyid = tryparse(SurveysController.SurveyID, payload(:survey), base=10)
    ResultsController.resultsfile(surveyid, payload(:format))
end

route("/results/:survey#([A-Za-z0-9]+)/:responsefile#([A-Za-z0-9]+\\.[a-z0-9]+)",
      named = :result) do
    surveyid = tryparse(SurveysController.SurveyID, payload(:survey), base=10)
    response, format = split(payload(:responsefile), '.')
    responseid = tryparse(SurveysController.ResponseID, response,
                          base=SurveysController.UID_ENCBASE)
    try
        ResultsController.resultsfile(surveyid, responseid, format)
    catch e
        if e isa MethodError || e isa AssertionError
            Router.error(404, currenturl(), MIME"text/html")
        else
            rethrow(e)
        end
    end
end
