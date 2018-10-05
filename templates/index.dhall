let h = ./html/package.dhall in
let List/map = http://prelude.dhall-lang.org/List/map in
let List/concat = http://prelude.dhall-lang.org/List/concat in
\( top :
{ total : Natural
, done : Natural
, progress : List (./type/ProgressBar)
})
->
./base.dhall (
h.div h.na (
  List/concat Text
  [ [ h.h3_ ["Overview"]
    , h.div
      [ h.class "alert alert-secondary" ]
      [ h.h4_
        [ "Active Jobs "
        , "[${Natural/show top.done }/${Natural/show top.total}]"
        ]
      , h.p_ [''
         Currently on the server we have run
         ${Natural/show top.done}
         out of ${Natural/show top.total} jobs.
         '']
      , h.hr_
      , ./bootstrap/progress top.progress
      ]
    ]
  ]
))