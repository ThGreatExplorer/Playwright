(
  (tmodule PointThreeE
    (class PointThreeE (x y z)
      (method delta ()
        (def x (this --> x))
        (def y (this --> y))
        (x + y)
      )
    )
    (
      ((x Number) (z Number) (y Number))
      ((delta (Number) Number))))
  (import PointThreeE)
  (def x 3.0)
  (def point (new Point (x x)))
  (point --> x = x)
  (x = (point --> delta (x)))
  x
)

