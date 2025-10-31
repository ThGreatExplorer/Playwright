(
  (module A (class C ())) 
  (module B (class C () (method val () 3.0))) 
    (import A) (import B) 
    (def x (new C ())) 
    (x --> val ())
)