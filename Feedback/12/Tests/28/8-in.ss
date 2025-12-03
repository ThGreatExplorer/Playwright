((module
   Bad
   (class Bad (x x) (method get () (this --> x))))
 (timport Bad (((x Number) (x Number)) ((get () Number))))
 (def five 5.0)
 (def b (new Bad (five five)))
 (b --> get ()))