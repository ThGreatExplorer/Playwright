(
  (class PointA (x y)
    (method addCoord () 2.0)
    (method setInternal (x) (x = 2.0) x)
  )

  (def one 1.0)
  (def two 2.0)

  (def pA (new PointA (one two)))
  (def x  (new PointA (one two)))

  (pA = (pA --> setInternal (x)))

  x)