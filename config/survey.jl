# SPDX-License-Identifier: CC-BY-SA-4.0
Survey(
    "Emacs User Survey — 2022 (Preview)",
    "Help the community have a better understanding of itself and its
        own diversity in Emacs usage. Discover and show how people are using
        this versatile tool for everything, from software engineering to
        academia and journalism.
<br/><br/>
This takes most people ?–? minutes to complete.
<br/><br/>
<b>Disclaimer:</b> This is currently just a draft based on the 2020 survey,
the questions you see now will not neccesarily be in the final survey.",
    SurveyPart("Emacs Usage",
        MultiSelect(:emacs_tasks,
            "Which of the following activities do you use Emacs for?",
            ["Work", "Hobby projects", :other]),
        NumberInput(:emacs_years,
            "How many years have you been using Emacs for?",
            validators = v -> if v < 0
                "Seriously? Are we supposed to believe you're just planning ahead?"
            elseif v > 45
                "Oh really? 🤨"
            end),
        RadioSelect(:emacs_version,
            "Which version of Emacs do you use?",
            ["29 (master/HEAD)" => "29.0",
            "28.2", "28.1", "27.2", "27.1", "26.3", :other],
            validators = v -> if isnothing(match(r"^\d+\.\d+$", v))
                "Please give the MAJOR.MINOR version number as your response"
            elseif parse(VersionNumber, v) < v"18"
                "Ok, this is pushing the bounds of believability a bit much..."
            elseif parse(VersionNumber, v) > v"29.0"
                "You should have mentioned that you're a time traveler!"
            end),
        MultiSelect(:emacs_motivations_current,
            "Which features keep you using Emacs?",
            ["Extensibility", "Package(s)", "Text editing features",
             "Built-in features (dired, calc, etc.)" => "Built-in features",
             "Community", "Part of the GNU project / FSF",
             :other],
            mandatory=false),
        MultiSelect(:operating_system,
            "Which operating system to you use Emacs on?",
            ["GNU/Linux",
             "Linux via <abbr title=\"Windows Subsystem for Linux\">WSL</abbr>" => "WSL",
             "Windows", "MacOS", "BSD", :other]),
        MultiSelect(:emacs_mode,
            "How do you run Emacs?",
            ["Graphical Application (GUI)" => "GUI",
             "Terminal (TUI)" => "TUI",
             "Daemon/Client" => "daemon"]),
        RadioSelect(:emacs_performance,
            "How well does Emacs perform in your experience?",
            ["Very well, it's snappy", "Good but not great",
             "Alright", "Not well", "Poorly, it's slugish"]),
        RadioSelect(:keybindings,
            "Which keybindings do you use?",
            ["Default (Emacs)" => "Emacs", "Vim", "CUA", :other]),
        RadioSelect(:starter_kit,
            "Do you use a starter kit/configuration framework?",
            ["No, I have lightly configured vanilla Emacs" => "minimal vanilla",
             "No, I have an extensive, fully-custom configuration" => "maximal vanilla",
             "Doom Emacs", "Spacemacs", "Prelude",
             :other]),
        RadioSelect(:elisp,
            "What is your level of Elisp proficiency?",
            ["No knowledge" => "none",
             "I can copy-paste and tweak some code" => "copy-paste",
             "I can read and understand most Elisp programmes" => "read and understand",
             "I can write simple functions" => "simple functions",
             "Fairly proficient, I can/have written my own package" => "write package",
             "I maintain multiple packages" => "many packages",
             "I have a solid understanding of Emacs internals" => "understand internals"]),
        MultiSelect(:languages,
            "Which languages do you program (in Emacs) in?",
            ["Assembly", "C++", "C", "C#", "Clojure", "Elixir",
             "Erlang", "Go", "HTML/CSS", "Haskell", "Javascript",
             "Julia", "Java", "Elisp", "Python", "PHP", "Perl",
             "R", "Rust", "Ruby", "Scheme", "Shell scripting" => "Shell",
             "SQL", "Typescript", "Prose (writing)" => "Prose", :other])),
    SurveyPart("First time questions",
        "All of these questions are <i>optional</i>. Answer these if you're taking the survey for the first time.",
        RadioSelect(:prior_editor,
            "Which editor did you use before you started using Emacs?",
            ["None", "Vim", "VS Code", "IntelliJ", "Atom",
             "Sublime Text", "Eclipse", "Nodepad++", :other],
            mandatory=false),
        TextArea(:emacs_strengths,
            "What do you think are Emacs' greatest strengths?",
            validators=[wordlimit(400), charlimit(6*400)],
            mandatory=false),
        TextArea(:emacs_dificulties,
            "Can you recall any difficulties you faced when initially learning Emacs?<br/>Please be as specific and concrete as your memories permit.",
            validators=[wordlimit(400), charlimit(6*400)],
            mandatory=false),
        RadioSelect(:emacs_introduction,
            "How were you introduced to Emacs?",
            ["Friend", "Recommended by a collegue" => "Collegue",
             "University/College", "Some part of the internet" => "Internet", :other],
            mandatory=false),
        MultiSelect(:emacs_motivations_initial,
            "Which features motivated you to initially try Emacs?",
            ["Extensibility", "Package(s)", "Text editing features",
             "Community", "Part of the GNU project / FSF",
             :other],
            mandatory=false)),
    SurveyPart("Packages",
        MultiSelect(:package_manager,
            "How do you manage third-party packages?",
            ["My framework/starter kit's default" => "framework default",
             "package.el (built-in)" => "package.el",
             "straight", "elpaca", "use-package",
             "manual (git clone/download)" => "manual",
             "not sure", :other]),
        MultiSelect(:package_archives,
            "Which package archives do you use?",
            ["My framework/starter kit's default" => "framework default",
             "GNU ELPA", "GNU ELPA devel", "NonGNU ELPA", "MELPA", "MELPA Stable",
             "Built from source (e.g. using straight)" => "Source",
             "not sure", :other]),
        MultiSelect(:package_discovery,
            "How do you learn about new packages?",
            ["Reddit", "GitHub", "Mailing lists",
             "IRC servers", "Matrix servers", "Discord servers",
             "Web search engines", "GNU ELPA/MELPA online package lists",
             "M-x list-packages", :other]),
        MultiSelect(:package_research,
            "When you discover a new package that looks interesting, what do you do next?",
            ["Install and try it immediately",
             "Research alternatives", "Wait for it to mature" => "wait for maturity",
             "Wait till I see other people in the community using it" => "wait for community"]),
        TextArea(:favorite_packages,
            "Could you list some of your favourite packages? (comma-seperated)";
            validators = [t -> if length(split(t, ',')) > 10
                    "That's a lot of packages. Surely you can cut that down to a top 10?"
                          end, wordlimit(30), charlimit(200)],
            mandatory=false),
        MultiSelect(:package_contact_method,
            "If you wish to contact the package maintainer(s), how do you do so?",
            ["Mailing list", "Email", "GitHub", :other]),
        MultiSelect(:package_help,
            "Where do you ask for help using packages?",
            ["IRC", "Matrix", "Reddit", "Emacs StackExchange",
             "My emacs framework's community" => "Framework",
             "GitHub", "Mailing lists", "Email the maintainer(s)" => "Email",
             "Discord", "Discourse", :other]),
        MultiSelect(:package_contibutions,
            "What contributions do you make to packages you use?",
            ["Issues", "Code (feqtures)", "Code (bugfixes)", "Documentation", "Advocacy"],
            mandatory=false),
        RadioSelect(:package_contibution_frequency,
            "How often do you contribute to packages?",
            ["Frequently", "Often", "Sometimes", "Rarely", "Never"]),
        MultiSelect(:package_contribution_barriers,
            "If you rarely/never contribute to packages, what are the main bariers to doing so?",
            ["Not interested",
             "Elisp proficiency",
             "Not knowing how to modify a package",
             "Not confident in my code quality" =>
                 "Lack confidence in code quality",
             "Lack of familiarity with the contribution process" =>
                 "Unfamiliar with contribution process",
             "Wariness of the review process",
             "Bad prior experience",
             "FSF paperwork (for GNU ELPA)" => "FSF paperwork",
             :other],
            mandatory=false),
        RadioSelect(:fsf_paperwork_year,
            "Have you signed (or attempted to sign) the FSF copyright papers?",
            ["No, never seriously considered" => "never considered",
             "No, but I might in the future" => "might in future",
             "Yes, within the last 2 years" => "0-2y",
             "Yes, 2-5 years ago" => "2-5y",
             "Yes, 5-10 years ago" => "5-10y",
             "Yes, 10+ years ago" => "10+y"]),
        TextArea(:package_submission,
            "If you write packages, how you you decide which repositories to submit it to (if any)?",
            mandatory=false),
        MultiSelect(:package_documentation_wish,
            "What documentation do you wish package authors would more often provide?",
            ["Comparison with alternatives" => "Comparison", "Tutorials",
             "Design rationale", "Info manual" => "Manual", "Screenshots",
             "Clips/videos", "Intoduction/overview" => "Overview",
             :other]),
        MultiSelect(:theme, # people can use a light and dark theme
            "Which theme do you use?",
            ["The default <i>light</i> theme" => "default light",
             "The default <i>dark</i> theme" => "default dark",
             "my own custom theme",
             "doom-one", "dracula", "gruvbox",
             "modus-operandi", "modus-vivendi", "nord",
             "solarized", "solarized-dark", "zenburn",
             :other]),
        MultiSelect(:org_usage,
            "Which use cases of Org Mode apply to your usage (if any)?",
            ["Note taking", "General document writing",
             "Task management", "Agenda", "Time tracking",
             "Personal knowledge base (PKB)" => "PKB",
             "Literate programing / notebooks (babel)" => "babel"],
            mandatory=false),
        RadioSelect(:magit_usage,
            "How often do you use Magit?",
            ["Frequently", "Often", "Sometimes", "Rarely", "Never"]),
        RadioSelect(:tramp_usage,
            "How often do you use Tramp?",
            ["Frequently", "Often", "Sometimes", "Rarely", "Never"]),
        MultiSelect(:term_package,
            "How do you interact with the shell in Emacs?",
            ["I don't" => "none", "shell-command", "shell",
             "eshell", "term", "ansi-term", "vterm", :other]),
        RadioSelect(:email_package,
            "Do you use an Email client in Emacs?",
            ["No", "Gnus", "Mu4e", "notmuch", "Wanderlust", "Rmail", :other]),
        RadioSelect(:spell_package,
            "Do you use a spell checker in Emacs?",
            ["No", "My framework/starter kit's default" => "framework default",
             "flyspell", "spell-fu", "ispell", :other]),
        RadioSelect(:undo_package,
            "Do you use an undo package in Emacs?",
            ["No", "My framework/starter kit's default" => "framework default",
             "undo-tree", "undo-fu", "vundo", :other]),
        RadioSelect(:project_package,
            "Do you use an project management package in Emacs?",
            ["No", "My framework/starter kit's default" => "framework default",
             "project.el", "projectile", :other]),
        MultiSelect(:selection_package,
            "Do you use any selection packages?",
            ["No", "My framework/starter kit's default" => "framework default",
             "Helm", "Ivy", "Ido", "Icomplete",
             "Vertico", "Selectrum", "Marginalia", "Consult",
             :other])),
    SurveyPart("Contribution",
        RadioSelect(:emacs_contribution,
            "Have you contributed to Emacs or a GNU ELPA package?",
            ["Frequently", "Often", "Sometimes", "Rarely", "Never"]),
        TextArea(:emacs_contribution_feedback,
            "Have you got any feedback on the Emacs/GNU ELPA contribution process?",
            validators=[wordlimit(400), charlimit(6*400)],
            mandatory=false),
        RadioSelect(:package_contribution,
            "Have you contributed to an Emacs package?",
            ["Frequently", "Often", "Sometimes", "Rarely", "Never"]),
        RadioSelect(:isa_package_maintainer,
            "Do you maintain any Emacs packages?",
            ["Yes", "No"]),
        RadioSelect(:has_donated,
            "Have you ever contributed financially to Emacs development (via the FSF)",
            ["No",
             "Yes",
             "I would if I could donate to Emacs directly" => "Would directly",
             "I would like to, but cannot" => "Cannot"])),
    SurveyPart("Demographics (all questions are optional)",
        NumberInput(:respondent_age,
            "How old are you?",
            validators = n -> if n < 8
                "My, you're advanced for you're age. <i>Suspisiously</i> advanced…"
            elseif n > 100
                "Congratulations on becoming a centernarian! How about you get one of your grandchildren to do this survey instead?"
            end,
            mandatory=false),
        RadioSelect(:respondent_gender,
            "What is your gender?",
            ["Male", "Female", "Other"],
            mandatory=false),
        RadioSelect(:respondant_education,
            "What is the highest level of education you have completed?",
            ["High school" => "School",
             "Associate's Degree" => "Associates",
             "Bachelor's Degree" => "Bachelors",
             "Master's Degree" => "Masters",
             "PhD or equivalent" => "PhD"],
            mandatory=false),
        Dropdown(:respondant_nationality,
            "What is your nationality?",
            # taken from https://www.gov.uk/government/publications/nationalities/list-of-nationalities
            ["(other)", "Afghan", "Albanian", "Algerian", "American", "Andorran", "Angolan", "Anguillan", "Antigua and Barbuda",
             "Argentine", "Armenian", "Australian", "Austrian", "Azerbaijani", "Bahamian", "Bahraini", "Bangladeshi",
             "Barbadian", "Belarusian", "Belgian", "Belizean", "Beninese", "Bermudian", "Bhutanese", "Bolivian",
             "Bosnia and Herzegovina", "Botswanan", "Brazilian", "British", "British Virgin Islander", "Bruneian",
             "Bulgarian", "Burkinan", "Burmese", "Burundian", "Cambodian", "Cameroonian", "Canadian", "Cape", "Verdean",
             "Cayman Islander", "Central", "African", "Chadian", "Chilean", "Chinese", "Colombian", "Comoran",
             "Congolese (Congo)", "Congolese (DRC)", "Cook Islander", "Costa", "Rican", "Croatian", "Cuban", "Cymraes",
             "Cymro", "Cypriot", "Czech", "Danish", "Djiboutian", "Dominican", "Dominican Republic citizen", "Dutch",
             "East Timorese", "Ecuadorean", "Egyptian", "Emirati", "English", "Equatorial", "Guinean", "Eritrean", "Estonian",
             "Ethiopian", "Faroese", "Fijian", "Filipino", "Finnish", "French", "Gabonese", "Gambian", "Georgian",
             "German", "Ghanaian", "Gibraltarian", "Greek", "Greenlandic", "Grenadian", "Guamanian", "Guatemalan",
             "Guinea-Bissau citizen", "Guinean", "Guyanese", "Haitian", "Honduran", "Hong", "Konger", "Hungarian", "Icelandic",
             "Indian", "Indonesian", "Iranian", "Iraqi", "Irish", "Israeli", "Italian", "Ivorian", "Jamaican", "Japanese",
             "Jordanian", "Kazakh", "Kenyan", "Kittitian", "Kiribati citizen", "Kosovan", "Kuwaiti", "Kyrgyz", "Lao",
             "Latvian", "Lebanese", "Liberian", "Libyan", "Liechtenstein citizen", "Lithuanian", "Luxembourger",
             "Macanese", "Macedonian", "Malagasy", "Malawian", "Malaysian", "Maldivian", "Malian", "Maltese",
             "Marshallese", "Martiniquais", "Mauritanian", "Mauritian", "Mexican", "Micronesian", "Moldovan", "Monegasque",
             "Mongolian", "Montenegrin", "Montserratian", "Moroccan", "Mosotho", "Mozambican", "Namibian", "Nauruan",
             "Nepalese", "New Zealander", "Nicaraguan", "Nigerian", "Nigerien", "Niuean", "North Korean", "Northern Irish",
             "Norwegian", "Omani", "Pakistani", "Palauan", "Palestinian", "Panamanian", "Papua", "New", "Guinean",
             "Paraguayan", "Peruvian", "Pitcairn Islander", "Polish", "Portuguese", "Prydeinig", "Puerto", "Rican",
             "Qatari", "Romanian", "Russian", "Rwandan", "Salvadorean", "Sammarinese", "Samoan", "Sao", "Tomean",
             "Saudi Arabian", "Scottish", "Senegalese", "Serbian", "Seychelles citizen", "Sierra", "Leonean",
             "Singaporean", "Slovak", "Slovenian", "Solomon Islander", "Somali", "South African", "South Korean",
             "South Sudanese", "Spanish", "Sri Lankan", "St Helenian", "St Lucian", "Stateless", "Sudanese",
             "Surinamese", "Swazi", "Swedish", "Swiss", "Syrian", "Taiwanese", "Tajik", "Tanzanian", "Thai", "Togolese",
             "Tongan", "Trinidadian", "Tristanian", "Tunisian", "Turkish", "Turkmen", "Turks and Caicos Islander",
             "Tuvaluan", "United States citizen" => "American",
             "Ugandan", "Ukrainian", "Uruguayan", "Uzbek",
             "Vatican citizen", "Vanuatu citizen", "Venezuelan", "Vietnamese", "Vincentian", "Wallisian", "Welsh",
             "Yemeni", "Zambian", "Zimbabwean"],
            mandatory=false),
        RadioSelect(:respondent_ocupation,
            "Which industry do you work in?",
            ["Academia (Student)" => "Student",
             "Academia/Research" => "Research",
             "Creative/Writing",
             "Education services", "FinTech",
             "Healthcare", "Legal", "Manufacturing",
             "Media/Publishing",
             "Software Development", "Telecom",
             :other],
            mandatory=false)),
    SurveyPart("Other software usage",
        RadioSelect(:free_software_zeal,
            "How strongly do the ideals of free/libre software affect your computer use?",
            ["I have a clear preference for free/libre software and am willing to accept <em>any</em> lack of features or UI polish",
             "I have a clear preference for free/libre software and am willing to accept a <em>significant</em> lack of features or UI polish",
             "I have a clear preference for free/libre software and am willing to accept a <em>limited</em> lack of features or UI polish",
             "I have a clear preference for free/libre software but am <em>not</em> willing to accept a lack of features or UI polish",
             "I have no preference for free/libre software"])),
    SurveyPart("Survey bookeeping",
        RadioSelect(:survey_prior,
            "Did you respond to the 2020 Emacs survey?",
            ["Yes", "No"]),
        RadioSelect(:survey_referrer,
            "How did you find out about this survey?",
            ["r/emacs", "Hacker News", "Direct message / friend" => "Friend",
             "Discord", "Discourse", "Twitter", "Blog post", "Emacs mailing list",
             "lobste.rs", "Telegram", "Emacs China", "IRC",
             :other]),
        RadioSelect(:survey_ux,
            "How well do you think this survey platform works?",
            ["Great", "Ok", "Not great"]),
        TextArea(:survey_feedback,
            "Do you have any general feedback on the survey?",
            validators=[wordlimit(200), charlimit(6*200)],
            mandatory=false)))
