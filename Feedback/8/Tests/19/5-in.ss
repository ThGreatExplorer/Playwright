((module C
    (class Cowboy ()
      (method draw ()
        1.0)))
 (module A
    (class Cowboy ()
      (method draw ()
        777.0)))
 (import C)
 (import A)
 (def x (new Cowboy ()))
 (x --> draw ()))
