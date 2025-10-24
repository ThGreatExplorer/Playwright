((class Cowboy ()
  (method draw()
   (def output 3.0)
    output))

(class Artist ()
  (method draw()
    777.0))

(def a (new Artist ()))
(def c (new Cowboy ()))
(def x -88.0)
(if0 x
     (x = a)
     (x = c))
(x --> draw()))