((def a 0.5) (if0 0.0 (block (def a 1.0) (a = (a + a))) (a = 0.0)) a)
