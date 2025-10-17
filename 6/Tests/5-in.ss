(
  (class PointA (x y)
    (method addToB (a b)
      (def tempX (this --> x))
      (def tempY (this --> y))
      (def tempA (tempX + a))
      (def tempB (tempY + b))
      (def instanceB (new PointB (tempA tempB)))
      instanceB
    )
  )
  (class PointB (a b))
  0.0
)