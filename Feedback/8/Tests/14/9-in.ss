((module Shapes (class Square (side)))

(module Colors (class Red ()))


(import Shapes)
(import Colors)

(def x 1.0)
(def s (new Square (x)))

(def r (new Red ()))
(def result 0.0)

(if0 (s isa Square)
    (block 
      (if0 (r isa Red)
        (block (result = 1.0))
        (block (result = 4.0))
      )
    )
    (block (result = 2.0))
  )

result)