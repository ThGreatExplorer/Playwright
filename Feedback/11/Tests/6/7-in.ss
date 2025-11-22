(
  (module Uone
    (class Cone ()
      (method a () 1.0)))
  (module Utwo
    (class Ctwo ()
      (method b () 2.0)))
  (module Uthree
    (class Cthree ()
      (method c () 3.0)))
  (tmodule T
    (timport Uone (() ((a () Number))))
    (timport Utwo (() ((b () Number))))
    (timport Uthree (() ((c () Number))))
    (class Main ()
      (method run () 0.0))
    (() ((run () Number))))
  (import T)
  0.0
)