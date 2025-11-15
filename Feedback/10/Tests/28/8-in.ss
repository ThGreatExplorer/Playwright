((module
  Data
  (class Data (x)
    (method get () (this --> x))))
 (tmodule
  User
  (timport Data (((x Number)) ((get () Number))))
  (class User ()
    (method test (d)
      (def obj (new Data (d)))
      obj))
  (() ((test (Number) Number))))
 (import User)
 (def one 1.0)
 (def u (new User ()))
 (u --> test (one)))