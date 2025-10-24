7/Tests/6-in.ss
^ Path to inegration test exposing the difference between WRONG PITCH and PITCH

((class While (count)
   (method w()
     (def d (this --> count))
     (def one -1.0)
     (def e (d + one))
     (if0 e
          (e = e)
          (this --> count = e))
     (this --> w ())))

 (def u 1000.0)
 (def w (new While (u)))
 (w --> w ()))

((class While ()
   (method w(other)
     (other --> w (this))))

 (class Repeat ()
   (method w(other)
     (other --> w (this))))

 (def r (new Repeat ()))
 (def w (new While ()))

 (w --> w(r)))