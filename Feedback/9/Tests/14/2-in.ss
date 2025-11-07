((tmodule PointThreeD
   (class Meow
     (x y)
     (method consume ()
       (def x (this --> x))
       (def y (this --> y))
       (x + y)))
       (((x Number) (y Number))
        (consume () Number))
       )
 (tmodule Triangle 
   (class Angles
     (a b c)
     (method area ()
       (def x (this --> a))
       (def y (this --> b))
       (def z (this --> c))

       (def sum (x + y))
       (sum + z)))
       (((a Number) (b Number) (c Number))
        ((area () Number))
       ))
 (import PointThreeD)
 (import Triangle)
 (def x 5.0)
 (def meow (new Meow (x x)))
 (meow --> x = x)
 (x = (meow --> consume ()))
 x)

