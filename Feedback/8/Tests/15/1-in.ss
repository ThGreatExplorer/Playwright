((module Cat (class meow
                 (x y)
                 (method delta (x) (def y (this --> y)) (x = 3.0) (x + y))))
 (module Cat (class meow
                 (x y f)
                 (method
                  woof
                  ()
                  (def x (this --> x))
                  (def y (this --> y))
                  (x + y))))
 (import Cat)
 (def x 5.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
