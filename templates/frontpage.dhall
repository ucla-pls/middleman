let h = ./html/package.dhall in
let List/map = http://prelude.dhall-lang.org/List/map in
let List/concat = http://prelude.dhall-lang.org/List/concat in
let Worker = { hostname : Text } in
let ActiveJobs =
  { succeded : Natural
  , failed : Natural
  , running : Natural
  , total : Natural
  , psucceded : Double
  , pfailed : Double
  , prunning : Double
  } in
let renderActiveJobs = \(aj : ActiveJobs) ->
h.div
[ h.class "alert alert-secondary" ]
[ h.h4_
  [ "Active Jobs [${Natural/show (aj.succeded + aj.failed)}/${Natural/show aj.total}]" ]
, h.p_ [ ''
  Currently on the server we have run ${Natural/show (aj.succeded + aj.failed)}
  out of ${Natural/show aj.total} jobs.
  '' ]
, h.hr_
, ./bootstrap/progress
  [ { class = "bg-danger", progress = aj.pfailed }
  , { class = "bg-success", progress = aj.psucceded }
  , { class = "bg-success progress-bar-striped", progress = aj.prunning }
  ]
]
in
let renderWorker = \(work : Worker ) ->
h.div [ h.class "alert alert-secondary" ] [ h.h3_ [ work.hostname ] ]
in
\(top : { activeJobs : ActiveJobs, workers : List Worker })
->
./index.dhall (
  h.div h.na (
    List/concat Text
    [ [ h.h3_ ["Overview"]
      , renderActiveJobs top.activeJobs
      ]
    , [ h.hr_
      , h.h3_ ["Workers"]
      ]
    , List/map Worker Text renderWorker top.workers
    ]
))