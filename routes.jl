using Genie.Router, Genie.Requests, Genie.Renderers.Html

using SurveysController
using ResultsController

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

route("/results(?:/(?:[A-Za-z0-9]+(?:\\.[a-z]+)?)?)?\$") do
    survey, format = match(r"([A-Za-z0-9]+)?(?:\.([a-z]+))?$", currenturl()).captures
    surveyid = tryparse(UInt32, survey, base=10)
    if !isnothing(format)
        ResultsController.resultsfile(surveyid, format)
    else
        ResultsController.resultsindex(surveyid)
    end
end
