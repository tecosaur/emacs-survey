module SurveysController

using Genie.Router, Genie.Requests, Genie.Renderers.Html, HTTP
using Random # For shuffling donation links

using ..Main.UserApp.Results, ..Main.UserApp.Surveys, Dates

const SURVEY = include("../../../config/survey.jl")
const INPROGRESS = Dict{Surveys.ResponseID, Surveys.Response}()

const UID_ENCBASE = 36

const RESUME_COOKIE_MAX_AGE = 60*60*24*2 # two days, in seconds

donesetup = false
function setup()
    global donesetup
    if !donesetup
        Surveys.set_current_survey!(SURVEY)
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
    function encrypted_xord_ip()
        ip_str = get(Dict(payload()[:REQUEST].headers), "X-Forwarded-For", "0.0.0.0")
        ip = parse.(UInt8, split(ip_str, '.'))
        @info "client ip: $ip"
        xor(reinterpret(UInt32, Genie.Encryption.encrypt(rand(UInt8, 4)) |> hex2bytes)...)
    end
    exip = encrypted_xord_ip()
    r = Surveys.Response(SURVEY,
                         vcat(responseids(SURVEY),
                              Vector{Surveys.ResponseID}(keys(INPROGRESS) |> collect));
                         exip)
    INPROGRESS[r.id] = r
    register!(r, exip)
    uid_str = string(r.id, base=UID_ENCBASE)
    Genie.Renderer.redirect(HTTP.URIs.URI(currenturl()).path * "?uid=$uid_str&page=1")
end

function completed(uid)
    uidstr = string(uid, base=UID_ENCBASE)
    links = map(["txt", "org", "json"]) do fmt
        (fmt, linkto(:result,
                     survey = string(SURVEY.id, base=10),
                     responsefile = string(uidstr, '.', fmt),
                     preserve_query = false))
    end
    res = html(:surveys, :thanks, layout=:base;
               uid=uidstr, survey=SURVEY, resultlinks=links,
               donateitems=donationlinks())
    Genie.Cookies.set!(res, "response-page", 0, Dict{String, Any}("maxage" => -1))
    Genie.Cookies.set!(res, "response-id", "", Dict{String, Any}("maxage" => -1))
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
            # Verify the survey is /actually/ complete,
            # and go to response's current page if not.
            if INPROGRESS[uid].page > length(SURVEY)
                @info SURVEY => INPROGRESS[uid]
                INPROGRESS[uid].completed = now()
                save!(INPROGRESS[uid], Symbol[])
                delete!(INPROGRESS, uid)
                completed(uid)
            else
                forminfo[:page] = string(INPROGRESS[uid].page)
                serve(forminfo)
            end
        else
            res = html(:surveys, :survey, layout=:base,
                       uid=uid_str, survey=SURVEY,
                       response=INPROGRESS[uid], page=page)
            Genie.Cookies.set!(res, "response-page", INPROGRESS[uid].page,
                               Dict{String, Any}("maxage" => RESUME_COOKIE_MAX_AGE,
                                                "httponly" => true))
            Genie.Cookies.set!(res, "response-id", uid_str,
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
            Genie.Renderer.redirect("/survey?uid=$uid_str&page=$page#formerror")
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

const DONATION_ENTITIES = [
    ("https://my.fsf.org/donate",
     "The Free Software Foundation",
     "https://www.fsf.org/",
     "principal organisational sponsor of the GNU project"),
    ("https://liberapay.com/org-mode/",
     "The Org Project",
     "https://orgmode.org",
     ["org-mode" => "https://orgmode.org"]),
    ("https://liberapay.com/hlissner/",
     "Henrik Lissner", "hlissner",
     ["Doom Emacs" => "doomemacs/doomemacs", "assorted packages"]),
    ("https://liberapay.com/magit/",
     "Jonas Bernoulli", "tarsius",
     ["Magit" => "magit", "Transient" => "transient", "others"]),
    ("https://github.com/sponsors/minad",
     "Daniel Mendler", "minad",
     ["Consult" => "https://github.com/minad/consult",
      "Vertico" => "https://github.com/minad/vertico",
      "Corfu" => "https://github.com/minad/corfu",
      "other UI packages"]),
    ("https://github.com/sponsors/abo-abo",
     "Oleh Krehel", "abo-abo",
     ["Swiper" => "swiper", "Hydra" => "hydra",
      "Avy" => "avy", "Lispy" => "Lispy"]),
    ("https://github.com/sponsors/progfolio",
     "Nicholas Vollmer", "progfolio",
     ["Straight" => "radian-software/straight.el",
      "Elpaca" => "elpaca", "Doct" => "doct"]),
    ("https://liberapay.com/protesilaos/",
     "Protesilaos Stavrou", "protesilaos",
     ["Modus themes" => "https://git.sr.ht/~protesilaos/modus-themes",
      "Denote" => "https://git.sr.ht/~protesilaos/denote",
      "others"]),
    ("https://github.com/sponsors/yyoncho",
     "Ivan Yonchovski", "yyoncho",
     ["lsp-mode" => "emacs-lsp/lsp-mode", "dap-mode" => "emacs-lisp/dap-mode",
      "and other LSP things"]),
    ("https://liberapay.com/wasamasa/",
     "Vasilij Schneidermann", "wasamasa",
     ["nov.el" => "https://depp.brause.cc/nov.el/",
      "circe" => "https://github.com/emacs-circe/circe",
      "eyebrowse" => "https://depp.brause.cc/eyebrowse",
      "shackle" => "https://depp.brause.cc/shackle"]),
    ("https://liberapay.com/tec",
     "me, Timothy", "tecosaur",
     ["this survey", "a few other things"])
]

donationlinks() = donate_spec_to_link.(shuffle(DONATION_ENTITIES))

function donate_spec_to_link((link, name, site, projects))
    string("<li><a href=\"$link\"><b>Donate to</b></a> ",
           name, ' ',
           if isnothing(site)
               ""
           elseif occursin("http", site)
               "<a href=\"$site\" target=\"_blank\" title=\"Homepage\" \
                 style=\"color: var(--secondary); display: inline-block; \
                         transform: scale(-1, 1)\">⎋</a>"
           else # assume GitHub username
               "<a href=\"https://github.com/$site\" target=\"_blank\" \
                 style=\"color: var(--secondary)\" \
                 title=\"GitHub profile\">@$site</a>"
           end,
           if projects isa String
               " ($projects"
           else
               " (responsible for " *
               join(map(projects) do proj
                        if proj isa String
                            proj
                        elseif proj isa Pair{String,String}
                            string("<a href=\"",
                                if occursin("http", proj[2])
                                    proj[2]
                                elseif occursin("/", proj[2])
                                    "https://github.com/$(proj[2])"
                                elseif !isnothing(site)
                                    "https://github.com/$site/$(proj[2])"
                                end,
                                "\" target=\"_blank\" title=\"project page\" \
                                    style=\"color: var(--h1-color)\">",
                                proj[1], "</a>")
                        else
                            ""
                        end
                    end, ", ", ", and ")
           end,
           ")</li>")
end

end
