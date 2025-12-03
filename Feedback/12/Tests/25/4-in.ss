((module A
    (class B ()
      (method c ()
        (new B ()))))

  (tmodule X
    (timport A (() ((c (Number) Number))))
    (class Y ()
      (method m ()
        (def m (new B ()))
        (def y 1.0)
        (def x (m --> c (y)))
        35.0))
    (() ((m () Number))))

  (timport A ( () ((c () Number))))
  (import X)
  (def q (new Y ()))
  (q --> m ()))