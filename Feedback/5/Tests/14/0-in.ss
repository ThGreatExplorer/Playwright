((def x 0.0)
 (def y 1.0)
 (def z 2.0)
 (while0 x
         (block (def x 1.0)
                (def z (x + 1.0))
                (while0 x
                        (block (def x 2.0)
                               (x = (x + x))))))
 z)