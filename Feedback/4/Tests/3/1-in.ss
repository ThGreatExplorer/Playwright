((def a 1.0)
  (if0 a (block (def b 2.0)) (a = a))
  a)
