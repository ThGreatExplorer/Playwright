((class Timer (time)
  (method getTime() (this --> time)))

(def three 3.0)
(def fourtyTwo 42.0)
(def true 0.0)
(while0 true
  (block
    (def c (new Timer (three)))
    (fourtyTwo = c)
    (true = 1.0)))
(fourtyTwo --> getTime()))