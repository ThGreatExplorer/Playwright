(  
   (module A
   (class G
   (o)
   (method
    small
    (delta n)
    (def k 0.0)
    (def neg -1.0)
    (while0 k (if0 n (k = 1.0) (block (delta = (delta / n)) (n = (n + neg)))))
    delta))
   )

   (module B
   (import A)
 (class H (g)
 (method m (param)
 (def val 0.0)
 (def prev (new G (val)))
 val
 )
 )
   )
   (import A)
   (import B)
 (def z 0.0)
 (def g (new G (z)))
 (def o 0.1)
 (def t 100.0)
 (def x (g --> small (o t)))
 (def h (new G (x)))
 (def a (new H (h)))
 (def b (new H (g)))
 (a == b))