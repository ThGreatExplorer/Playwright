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

  (if0 (pB == pA)
    (result = pB)
    (if0 (pA == pAA)
      (result = one)
      (if0 (two == pA)
        (result = two)
        (block
          (pAA --> x = (pA --> x))
          (pAA --> y = (pA --> y))
          (if0 (pA == pAA)
            (result = 413.0)
            (result = 612.0)
          )
        ))))

  result)