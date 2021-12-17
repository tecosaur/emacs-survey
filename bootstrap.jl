(pwd() != @__DIR__) && cd(@__DIR__) # allow starting app from bin/ dir

using EmacsSurvey
push!(Base.modules_warned_for, Base.PkgId(EmacsSurvey))
EmacsSurvey.main()
