( (module M (import N))       
  (module N (class C ())) 
  (import N)
  (def x 1.0)
  x
)