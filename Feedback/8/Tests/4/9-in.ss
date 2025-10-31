((module M
    (class M (x)
      (method make ()
        (def vx (this --> x))
        (new M (vx)))))
  (import M)
  (def one 1.0)
  (def obj (new M (one)))
  (obj --> make ()))