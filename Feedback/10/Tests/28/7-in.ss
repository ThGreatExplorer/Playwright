((module
  Data
  (class Data (x)
    (method get () (this --> x))))
 (tmodule
  User
  (import Data)
  (class User ()
    (method test ()
      (def one 1.0)
      one))
  (() ((test () Number))))
 (import User)
 (def u (new User ()))
 (u --> test ()))