(
  (module Cowboy (class C () (method draw () 1.0)))
  (module Artist (class C () (method draw () 2.0)))
  (module Knight (class C () (method draw () 8.0)))
 
  (import Artist)
  (import Knight)
  (import Cowboy)

  (def c (new C ()))
  (c --> draw ())
)