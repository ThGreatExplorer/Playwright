(
 (module base (class C (x y)))

 (module broken (import base) (class Creator () (method make () (def four 4.0) (new C (four four)))))

 (timport broken ( () ( (make () ( ( (x Number) (y ( () () )) ) ())))))
 (def c (new Creator ()))
 (def break (c --> make ()))
 4.0
 )