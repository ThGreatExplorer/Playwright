((def x 0.0)
 (while0 x
         (block (def x 1.0)
                (while0 x
                        (block (def x 2.0)
                               (x = (x + x))))))
 x)