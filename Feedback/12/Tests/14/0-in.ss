((tmodule
  A
  (class A (x)
    (method getX () (this --> x)))
  (((x Number)) ((getX () Number))))
 (tmodule
  B
  (timport A (((x Number)) ((getX () Number))))
  (class B ()
    (method makeA ()
      (def one 1.0)
      (new A (one))))
  (()
   ((makeA () (((x Number)) ((getX () Number)))))))
 (import A)
 (import B)
 (def b (new B ()))
 (b --> makeA ()))