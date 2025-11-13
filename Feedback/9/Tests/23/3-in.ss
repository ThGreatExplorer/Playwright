((tmodule a (class A () (method f () 4.0)) (() ((f () Number)))) (tmodule b (class B () (method g () 4.0)) (() ((g () Number)))) (import a) (import b) (def a (new A ())) (a = (new B ())) 0.1)
