((module A (class While ()
   (method w(other)
     (other --> w (this)))))

 (module B (class Repeat (limit)
   (method w(other)
     (def zero 0.0)
     (def one -1.0)
     (def curLimit (this --> limit))
     (if0 curLimit
         (this --> limit = (curLimit + one))
         (zero = (one / zero)))
         
    (other --> w (this)))))

    (import A)
    (import B)

    (def u 500.0)
    (def r (new Repeat (u)))
    (def w (new While ()))

    (w --> w(r)))