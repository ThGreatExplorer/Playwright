((module A
    (class B ()
      (method c ()
        (new B ()))))

  (tmodule X
    (timport A (() ((c () Number))))
    (class Y ()
      (method m ()
        (def m (new B ()))
        (m --> c ())))
    ( () ((m () Number))))

  (timport A ( () ((c () Number))))
  (import X)
  (def q (new Y ()))
  (q --> m ()))
