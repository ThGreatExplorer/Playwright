(
    (class A (x y) (method f (x y) (x = 0.0) y))
    (class B (x y) (method f (x y) (def x 0.0) y))
    (def a (new A ()))
    (def b (new B (a)))
    (b --> lol = b)
    (b --> a)
)