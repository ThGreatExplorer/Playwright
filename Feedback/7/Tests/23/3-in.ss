(
  (class C (x)
    (method add (x) 
      (def f (this --> x))
      (x + f)
    )
  )

(def ten 10.0)
(def five 5.0)
  (def c (new C (ten)))
  (c --> add (five))
)
