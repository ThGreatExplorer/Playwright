((module Lib
  (class Lib
    (x)
    (method inc ()
      (def one 1.0)
      (def tmp (this --> x))
      (tmp + one))))
 (tmodule
  User
  (timport Lib (((x Number)) ((inc () Number))))
  (class User
    ()
    (method do (n)
      (def l (new Lib (n)))
      (l --> inc ())))
  (() ((do (Number) Number))))
 (module Helper
  (import User)
  (class Helper
    ()
    (method use (n)
      (def u (new User ()))
      (u --> do (n)))))
 (timport Helper (() ((use (Number) Number))))
 (def start 2.0)
 (def h (new Helper ()))
 (def res (h --> use (start)))
 res)
