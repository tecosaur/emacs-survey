module SurveysController

using Genie.Router, Genie.Requests, Genie.Renderers.Html, HTTP

using Results, Surveys, Dates

const SURVEY = include("../../../config/survey.jl")
const INPROGRESS = Dict{Surveys.ResponseID, Surveys.Response}()

const UID_ENCBASE = 36

const RESUME_COOKIE_MAX_AGE = 60*60*24*2 # two days, in seconds

donesetup = false
function setup()
    global donesetup
    if !donesetup
        register!(SURVEY)
        for rid in responseids(SURVEY.id, :incomplete)
            INPROGRESS[rid] = Results.response(SURVEY.id, rid)
        end
        donesetup = true
    end
end

function index()
    setup()
    request = Genie.Router.params()[:REQUEST]
    responseid = Genie.Cookies.get(request, "response-id")
    html(:surveys, :index, layout=:base;
         survey=SURVEY, responseid)
end

function new()
    r = Surveys.Response(SURVEY, vcat(responseids(SURVEY),
                                      Vector{Surveys.ResponseID}(keys(INPROGRESS) |> collect)))
    INPROGRESS[r.id] = r
    register!(r)
    uid_str = string(r.id, base=UID_ENCBASE)
    Genie.Renderer.redirect(HTTP.URIs.URI(currenturl()).path * "?uid=$uid_str&page=1")
end

function completed(uid)
    res = html(:surveys, :thanks, layout=:base;
               uid=string(uid, base=UID_ENCBASE), survey=SURVEY)
    Genie.Cookies.set!(res, "response-id", "", Dict{String, Any}("maxage" => -1))
    Genie.Cookies.set!(res, "response-page", 0, Dict{String, Any}("maxage" => -1))
end

function serve(forminfo::Dict)
    setup()
    uid = if haskey(forminfo, :uid)
        tryparse(Surveys.ResponseID, forminfo[:uid], base=UID_ENCBASE)
    end
    if haskey(forminfo, :uid) && !haskey(INPROGRESS, uid) &&
        uid ∈ Results.responseids(SURVEY.id)
        # response has been completed
        completed(uid)
    elseif !haskey(forminfo, :uid) || !haskey(INPROGRESS, uid)
        clear = if haskey(forminfo, :clear)
            tryparse(Surveys.ResponseID, forminfo[:clear], base=UID_ENCBASE)
        end
        if haskey(INPROGRESS, clear)
            deregister!(INPROGRESS[clear])
            delete!(INPROGRESS, clear)
        end
        new()
    elseif !haskey(forminfo, :page)
        Genie.Renderer.redirect(string(currenturl(),
                                       "&page=", INPROGRESS[uid].page))
    else
        uid_str = string(uid, base=UID_ENCBASE)
        page = parse(Int, forminfo[:page])
        if page > length(SURVEY)
            @info SURVEY => INPROGRESS[uid]
            INPROGRESS[uid].completed = now()
            save!(INPROGRESS[uid], Symbol[])
            delete!(INPROGRESS, uid)
            completed(uid)
        else
            res = html(:surveys, :survey, layout=:base,
                       uid=uid_str, survey=SURVEY,
                       response=INPROGRESS[uid], page=page)
            Genie.Cookies.set!(res, "response-id", uid_str,
                               Dict{String, Any}("maxage" => RESUME_COOKIE_MAX_AGE,
                                                "httponly" => true))
            Genie.Cookies.set!(res, "response-page", INPROGRESS[uid].page,
                               Dict{String, Any}("maxage" => RESUME_COOKIE_MAX_AGE,
                                                "httponly" => true))
        end
    end
end

function submit(forminfo::Dict; backpage::Bool=false)
    uid = parse(Surveys.ResponseID, forminfo[:uid], base=UID_ENCBASE)
    page = parse(Int, forminfo[:page])
    data = reduce(function(acc::Dict, datum::Pair)
                      key, value = datum
                      if key ∈ (:uid, :page, :debug)
                      elseif match(r"\[\]$", string(key)) |> !isnothing
                          pkey = Symbol(replace(string(key), r"\[\]$" => ""))
                          acc[pkey] = value
                      elseif match(r"__override$", string(key)) |> !isnothing
                          pkey = Symbol(replace(string(key), r"__override$" => ""))
                          acc[pkey] = vcat(value)
                      else
                          acc[key] = vcat(value)
                      end
                      acc
                  end, forminfo, init=Dict{Symbol, Any}())
    # Questions on this page with no data are unanswered, but should be updated anyway
    for qid in getproperty.(SURVEY[page].questions, :id)
        if !haskey(data, qid)
            data[qid] = missing
        end
    end
    if forminfo[:debug] != "yes"
        uid_str = string(uid, base=UID_ENCBASE)
        response = INPROGRESS[uid]
        update!(response, SURVEY, data)
        @info SURVEY[page] => response
        if backpage || isvalid(response, SURVEY[page])
            response.page = if backpage
                max(1, page - 1)
            else
                max(response.page, page + 1)
            end
            save!(response, SURVEY[page])
            Genie.Renderer.redirect("/survey?uid=$uid_str&page=$(response.page)")
        else
            save!(response, SURVEY[page])
            Genie.Renderer.redirect("/survey?uid=$uid_str&page=$page")
        end
    else
        io = IOBuffer()
        ioc = IOContext(io, :limit => true, :displaysize => (80, 100))
        show(ioc, "text/plain", data)
        postdata = String(take!(io))
        html("""
        <!DOCTYPE html>
        <html lang="en">
            <head>
                <meta charset="utf-8">
                <meta http-equiv="X-UA-Compatible" content="IE=edge">
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <title>Emacs User Survey</title>
                <link href="/css/style.css" rel="stylesheet">
            </head>
            <body>
                <header>Data from page $page, response $(sprint(show, uid))</header>
                <main><pre><code> $(postdata) </code></pre></main>
                <footer>
                    This work is licensed under a
                    <a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/">CC-BY-SA 4.0 License</a>
                </footer>
            </body>
        </html>
        """)
    end
end

end
