(
  (tmodule Utils
    (class U ()
      (method id () 1.0))
    ( () ((id () Number)) ))

  (tmodule D
    (import Utils)
    (class Data (a b)
      (method read () (this --> a)))
    ( ((a Number) (a Number) (b Number))   
      ((read () Number)) ))

  (import D)
  (def one 1.0)
  (def two 2.0)
  (def d (new Data (one two)))
  0.0
)
