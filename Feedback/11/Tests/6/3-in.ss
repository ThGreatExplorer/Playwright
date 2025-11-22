(
  (module U (class A () (method f () (def x 1.0) x)))
  (tmodule T (timport U (() ((f () Number))))
    (class Main () (method run () 0.0))
    (() ((run () Number)))
  )
  (import T)
  0.0
)