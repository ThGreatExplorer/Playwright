((tmodule M
   (class C (val)
     (method get () (this --> val)))
   (((val Number))
    ((get () Number))))
 (import M)
 (def value 7.5)
 (def c (new C (value)))
 (c --> get ()))