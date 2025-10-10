((def a 0.0)
 (def b 5.0)
 (def c 10.0)
 (def one 1.0)
 (while0 a
   (block
     (def b 20.0)
     (def d 30.0)
     (c = (b + d))
     (if0 one
       (block
         (def c 100.0)
         (b = c))
       (block
         (def e 40.0)
         (b = (b + e))))
     (a = one)))
 (b + c))