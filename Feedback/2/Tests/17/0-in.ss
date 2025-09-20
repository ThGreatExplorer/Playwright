(
  (a = 0.0)
  (b = a)

  (if0 a
       (block
         (f = -1000.0)
         (g = (c + d))
         (if0 (f == g)
              (block (n = (n + a)))
              (block (o = (o + b))))
         (while0 (g == f)
                 (block (p = (p + a)))))
       (block
         (h = 1000.0)))
  (while0 (a == b)
          (block
            (a = (a + b))
            (b = (a / c))))
  (if0 (g == g)
       (j = (a + b))     
       (block (k = (j / a)) (l = (k + j))))
  (q = (n == p))
  m
)
