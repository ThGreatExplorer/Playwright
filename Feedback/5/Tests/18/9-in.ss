(
  (def a 1.0)
  (def x 2.0)
  (def l 2.0)
  (x = 0.0)
  (a = 0.0)

  (while0 (x == l)
    (block
      (def a 4.0)
      (def b -1.0)
      (def c 9.0)
      (a = c)
      (x = 0.0)
      (b = (b + a))

      (while0 x
        (block
          (def d 2.0)
          (def e 3.0)
          (a = (d + e))
          (d = 0.0)
          (l = 3.0)

          (while0 d
            (block
              (def f 5.0)
              (f = (f + e))
              (d = 1.0)
              (x = 3.0)
              ))
          (a = 0.0)
          ))
      ))
  a
)
