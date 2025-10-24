(
  (class PointA (x y)
    (method addCoord () 2.0)
  )
  (class PointB (x y)
    (method addCoord () 1.0)
  )

  (def one 1.0)
  (def two 2.0)

  (def pA (new PointA (one two)))
  (def pAA (new PointA (two one)))
  (def pB (new PointB (one two)))

  (def result -1.0)

  (pB = pAA)
  (if0 (pB isa PointB)
    (result = pB)
    (result = (pB --> addCoord (pA)))
  )

  result)