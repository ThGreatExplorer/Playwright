((tmodule a (class A ()) (() ())) (tmodule b (import a) (class B () (method f () (new A ()))) (() ((f () Number)))) 0.1)
