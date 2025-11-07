((tmodule
  While
  (class While
    (x)
    (method
     while
     ()
     (def w (this --> x))
     (if0
      (this --> x)
      (w = (w / w))
      (block (def D -1.0) (this --> x = (w + D))))
     (this --> while ())))
  (((x Number)) ((while () Number))))
 (import While)
 (def three 3.0)
 (def w (new While (three)))
 (w --> while ()))
