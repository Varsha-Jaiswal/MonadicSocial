module ContentStrategy
  ( topics,
    generateMessageContent,
  )
where

import Types (Topic)

-- | List of trending real-world topics.
topics :: [Topic]
topics =
  [ "AI_Revolution",
    "Climate_Change",
    "Bitcoin_Surge",
    "SpaceX_Launch",
    "Remote_Work",
    "Election_Updates",
    "Haskell",
    "MSc_Thesis",
    "Trump_Presidency",
    "Phd_Application_Process",
    "QMUL",
    "Oxford_Phd_Interview"
  ]

-- | Generates a realistic message content based on the topic.
generateMessageContent :: Topic -> Int -> String
generateMessageContent topic seed =
  let templates = case topic of
        -- Tech & AI
        "AI_Revolution" ->
          [ "Honestly, ChatGPT is writing better code than me at this point, kinda clear.",
            "I'm worried about the alignment problem, nobody seems to take it seriously.",
            "Just saw a demo of the new model, it's actually insane how fast it's moving."
          ]
        "Bitcoin_Surge" ->
          [ "Bro, did you see the charts this morning? It's actually pumping.",
            "I wish I bought more when it was low, huge regret right now.",
            "Everyone is talking about crypto again, feels like the bull run is back."
          ]
        "SpaceX_Launch" ->
          [ "Staying up to watch the Starship launch, fingers crossed it doesn't explode.",
            "The engineering behind those boosters landing is just mind-blowing.",
            "I really hope we actually get to Mars in our lifetime, that would be sick."
          ]
        "Remote_Work" ->
          [ "I honestly can't imagine going back to the office 5 days a week.",
            "My back is killing me, I need to invest in a proper ergonomic chair.",
            "The freedom of working from anywhere is just unmatched, love it."
          ]
        -- Academic & Thesis (QMUL/Oxford)
        "MSc_Thesis" ->
          [ "I am so behind on my literature review, sending help immediately.",
            "My supervisor just completely tore apart my draft, back to square one.",
            "Just need to grind out these last few chapters and I'm free."
          ]
        "Phd_Application_Process" ->
          [ "Writing these statements of purpose is draining my soul.",
            "Trying to find funding is harder than the actual research, it's a joke.",
            "Sent out 5 applications today, now the waiting game begins."
          ]
        "QMUL" ->
          [ "The library at Mile End is absolutely packed today, can't find a seat.",
            "Walking down the canal after lectures is the only thing keeping me sane.",
            "Actually really enjoying the Functional Programming module, Haskell is kinda cool."
          ]
        "Oxford_Phd_Interview" ->
          [ "I have my interview at Oxford next week and I'm absolutely terrified.",
            "Prepping for this interview is intense, they ask the wildest questions.",
            "Imagine actually getting into Oxford though, that would be life changing."
          ]
        "Haskell" ->
          [ "Once you understand monads, everything else just feels messy.",
            "I spent 3 hours debugging a type error and it was a missing bracket.",
            "Functional programming breaks your brain at first, but then it makes total sense."
          ]
        -- Politics & Society
        "Climate_Change" ->
          [ "The weather has been so weird lately, climate change is definitely real.",
            "I'm trying to cut down on meat, just doing my small part you know.",
            "It's scary thinking about what the world will look like in 50 years."
          ]
        "Trump_Presidency" ->
          [ "Did you see the latest tweet? It's all over my feed right now.",
            "The polls are so close, it's going to be a stressful election night.",
            "Politics is just so polarizing these days, distinct vibe shift."
          ]
        "Election_Updates" ->
          [ "I'm just glued to the news right now, waiting for the results.",
            "It's wild how much this election matters for the global economy.",
            "Make sure you go out and vote, it actually makes a difference."
          ]
        -- Default / Catch-all
        _ ->
          [ "Hey, do you have a sec to chat?",
            "It's been a long week, looking forward to the weekend.",
            "Just caught up on some sleep, feeling way better."
          ]
      idx = seed `mod` length templates
   in templates !! idx
