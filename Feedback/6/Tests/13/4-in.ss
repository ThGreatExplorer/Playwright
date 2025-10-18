(
  (class Point (x y)
    (method getX () x)
    (method getY () y)
    (method addX (n)
      (def temp (x + n))
      (x = temp)
      x
    )
  )
  (def p (new Point ()))
  (p --> x = 3.0)
  (p --> addX (x))
)