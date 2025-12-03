(

(module A (class A (num)))

(tmodule B (class B (num)) ( ( (num Number) ) ()))

(module compare (import A) (import B)
  (class Compare () (method compare ()
                            (def five 5.0)
                            (def f 5.0)
                            (def a (new A (five))) (def b (new A (f)))
                            (a == b))))

(timport compare ( () ( (compare () Number) ) ))
(def c (new Compare ()))
(c --> compare ())
 )