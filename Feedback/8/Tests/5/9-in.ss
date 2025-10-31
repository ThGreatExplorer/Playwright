(
  (module Math (class Calc () (method divide (x) (def zero 0.0) (x / zero))))

  (import Math)
  (def c (new Calc ()))
  (def five 5.0)
  (c --> divide (five))
)