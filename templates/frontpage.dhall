let h = ./html/package.dhall in
let List/map = http://prelude.dhall-lang.org/List/map in
let List/concat = http://prelude.dhall-lang.org/List/concat in
let Worker = { hostname : Text, workerActiveJobs : Natural } in
let ActiveJobs =
  { succeded : Natural
  , failed : Natural
  , running : Natural
  , total : Natural
  , psucceded : Double
  , pfailed : Double
  , prunning : Double
  }
in let renderActiveJobs = \(aj : ActiveJobs) ->
h.div
[ h.class "alert alert-secondary" ]
[ h.h4_
  [ "Active Jobs "
  , "[${Natural/show (aj.succeded + aj.failed)}/${Natural/show aj.total}]"
  ]
, h.p_ [''
  Currently on the server we have run ${Natural/show (aj.succeded + aj.failed)}
  out of ${Natural/show aj.total} jobs.
  '']
, h.hr_
, ./bootstrap/progress
  [ { class = "bg-danger", progress = aj.pfailed }
  , { class = "bg-success", progress = aj.psucceded }
  , { class = "bg-success progress-bar-striped", progress = aj.prunning }
  ]
]
in let renderWorker = \(work : Worker ) ->
h.a
[ h.class "list-group-item list-group-item-action flex-column align-items-start" ]
[ h.div [ h.class "d-flex w-100 justify-content-between" ]
  [ h.h5_ [ work.hostname ]
  , h.small_ [ "${Natural/show work.workerActiveJobs} active jobs" ]
  ]
, h.p [ h.class "mb-1" ]
  [ "Hello some examle" ]
]
in \(top : { activeJobs : ActiveJobs, workers : List Worker })
->
./index.dhall (
  h.div h.na (
    List/concat Text
    [ [ h.h3_ ["Overview"]
      , renderActiveJobs top.activeJobs
      ]
    , [ h.hr_
      , h.h3_ ["Workers"]
      , h.div [ h.class "list-group" ]
        ( List/map Worker Text renderWorker top.workers )
      ]
    ]
))