(
  (tmodule BOX
    (class Box (x)
      (method incr (a) a))
    (((x Number))
     ((incr (Number) Number))))
  (import BOX)

  (def x 1.0)
  (def b (new Box (x)))
  (def aa 1.0)
  (def bb 2.0)

  (b --> incr (aa bb))   ; expects 1, got 2
)
