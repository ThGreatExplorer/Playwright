(
   (module A
   (class While
   (x)
   (method
    w
    (other)
    (def w (this --> x))
    (if0
     (this --> x)
     (w = (w / w))
     (block (def D -1.0) (this --> x = (w + D))))
    (other --> w (this))))
   )
   (module B
 (class Repeat () (method w (other) (other --> w (this))))
   )
   (import A)
   (import B)
 (def x 2.0)
 (def r (new Repeat ()))
 (def w (new While (x)))
 (w --> w (r)))