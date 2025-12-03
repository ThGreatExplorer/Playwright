((module
  Bad
  (class Bad (x)
    (method use ()
      this)))
 (tmodule
  User
  (timport Bad (((x Number)) ((use () Number))))
  (class User ()
    (method run (b)
      (b --> use ())))
  (() ((run ((((x Number)) ((use () Number)))) Number))))
 (timport Bad (((x Number)) ((use () Number))))
 (import User)
 (def zero 0.0)
 (def bad (new Bad (zero)))
 (def user (new User ()))
 (user --> run (bad)))