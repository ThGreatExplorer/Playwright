((module X (class X (x)))
  (module A (class A () (method a (ax) ax)))
  (module B (import A) (class B () (method b (bx) (def A (new A ())) (A --> a (bx)))))
  (module C (import B) (class C () (method c (cx) (def B (new B ())) (B --> b (cx)))))
  (module D (import C) (class D () (method d (dx) (def C (new C ())) (C --> c (dx)))))
  (tmodule E
     (timport D (()
        (
            (d (
                (((x Number)) ())
               )
               Number
            )
        )
     ))
     (class E (ex) (method e () (def D (new D ())) (def ex (this --> ex)) (D --> d (ex))))
     (((ex (((x Number)) ()))) ((e () Number))))
  (import E)
  (timport X (((x Number)) ()))
  (def zero 0.0)
  (def X (new X (zero)))
  (def e (new E (X)))
  (e --> e ()))