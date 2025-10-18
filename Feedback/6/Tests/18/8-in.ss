(
  (class point (x y)
    (method move (dx dy)
      (def nx (x + dx))
      (def ny (y + dy))
      (x = nx)
      (y = ny)
      x))
  (class counter ()
    (method inc (v) v))
  (def dx 1.0)
  (def dy 2.0)
  (def p (new point (dx dy)))
  (p --> x = (p --> x))
  (def sx (p --> x))
  (def sy (p --> y))
  (def tmp (sx + sy))
  (while0 dx
    (block
      (def t 0.0)
      (dx = dy)))
  tmp
)

