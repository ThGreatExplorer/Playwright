((module A
    (class B ()
      (method c ()
        (new B ()))))

  (tmodule Body
    (timport CheckDefinedKeyWords (() ((c () Number))))
    (class Y ()
      (method m ()
        (def m (new B ()))
        (m --> c ())))
    ( () ((m () Number))))

  (timport A ( () ((c () Number))))
  (import X)
  (def q (new Y ()))
  (q --> m ()))