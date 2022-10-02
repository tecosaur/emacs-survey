(pwd() != @__DIR__) && cd(@__DIR__) # allow starting app from bin/ dir

using EmacsSurvey
const UserApp = EmacsSurvey
EmacsSurvey.main()
