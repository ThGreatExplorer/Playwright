((def a 0.0)
  (if0 a
        (block (def b 1.0) (a = a))
        (block (def c 2.0) (a = a)))
  a)

