((class point (x y)
   (method bumpswap (t)
     (def oldx (this --> x))
     (def oldy (this --> y))
     (def tmp oldx)
     (if0 t
          (block
            (this --> x = oldy)
            (this --> y = tmp))
          (block
            (this --> x = oldx)))
     (while0 t
       (block
         (this --> x = (this --> x))
         (t = 1.0)))
     (oldx + oldy)))
 (def one 1.0)
 (def zero 0.0)
 (def x 3.0)
 (def y 4.0)
 (def p (new point (x y)))
 (def res (p --> bumpswap (zero)))
 (res + one))
