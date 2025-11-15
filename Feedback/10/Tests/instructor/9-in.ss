((module While (class While
                 (x)
                 (method
                  while
                  ()
                  (def w (this --> x))
                  (if0
                   (this --> x)
                   (w = (w / w))
                   (block (def D -1.0) (this --> x = (w + D))))
                  (this --> while ()))))
 (timport While (((x Number)) ((while () Number))))
 (def three 3.0)
 (def w (new While (three)))
 (w --> while ()))
