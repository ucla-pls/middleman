let h = ./html/package.dhall
in 
\( body : Text )
->
  "<!doctype html>" ++ h.html h.na
  [ h.head h.na
    [ h.meta [ h.charset "utf-8" ] 
    , h.meta [ h.name "viewport", h.content "width=device-width, initial-scale=1, shrink-to-fit=no"]
    , h.link 
      [ h.href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
      , h.rel "stylesheet" 
      ] ( [] : List Text )
    ]  
  , h.body h.na 
    [ h.div [ h.class "container" ] 
      [ h.h1 [ h.style "margin-top: 1rem;" ] [ "Middleman"]
      , h.p_
        [ "The middleman make sure that the derivations gets build."
        ]
      , h.hr_
      , body
      ]
    , h.script 
      [ h.src "https://code.jquery.com/jquery-3.3.1.slim.min.js" 
      , h.integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" 
      , h.crossorigin "anonymous"
      ]
    , h.script
      [ h.src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" 
      , h.integrity "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" 
      , h.crossorigin "anonymous"
      ]
    , h.script
      [ h.src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" 
      , h.integrity "sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" 
      , h.crossorigin "anonymous"
      ]
    ]
  ]
