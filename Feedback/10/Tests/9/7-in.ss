( (tmodule M
    (class C ()
      (method v () 3.0))
      (() ((v () Number))))
  
  (import M)
  (import Food)   
  
  (def x 10.0)
  (def b 1.0)
  (x + b)
)
