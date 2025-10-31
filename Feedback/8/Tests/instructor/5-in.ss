((module While (class While
                 (x)
                 (method
                  w
                  (other)
                  (def w (this --> x))
                  (if0
                   (this --> x)
                   (w = (w / w))
                   (block (def D -1.0) (this --> x = (w + D))))
                  (other --> w (this)))))
 (module Repeat (class Repeat () (method w (other) (other --> w (this)))))
 (import While)
 (import Repeat)
 (def x 2.0)
 (def r (new Repeat ()))
 (def w (new While (x)))
 (w --> w (r)))
