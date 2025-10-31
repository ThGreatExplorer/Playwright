( 
    (module SomeM 
    (class SomeClass (o) 
        (method small (delta n)
            (def k 0.0)
            (def neg -1.0)
            (while0 k 
                (if0 n (k = 1.0) (block (delta = (delta / n)) (n = (n + neg)))))
            delta)))
    (module AnotherM (class AnotherClass (g)))

    (import SomeM)
    (import AnotherM)

    (def z 0.0)
    (def g (new SomeClass (z)))
    (def o 0.1)
    (def t 100.0)
    (def x (g --> small (o t)))
    (def h (new SomeClass (x)))
    (def a (new AnotherClass (h)))
    (def b (new AnotherClass (g)))

    (a == b)
)
