((class C (v)
  (method checkSelf ()
    (if0 this
      (this --> v = 1.0)
      (this --> v = 88.0))
    0.0
  )
)
(def zero 0.0)
(def c (new C (zero)))
(def garbage (c --> checkSelf ()))
(c --> v))
