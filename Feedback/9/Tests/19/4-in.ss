(
  (tmodule PAIR
    (class Pair (x y)
      (method sety (v) (this --> y = v) v))
    (((x Number) (y Number)) ((sety (Number) Number))))
  (import PAIR)
  (def vx 1.0)
  (def vy 2.0)
  (def p (new Pair (vx vy)))
  (def o (new Pair (vx vy)))
  (p --> sety (o))    ; sety expects Number, but we pass an object (o) → type error
)
