(
    (class Box (val)
        (method set (x) (this --> val = x) 0.0)
        (method get () (this --> val)))

    (def one 1.0)
    (def fourtytwo 42.0)
    (def b (new Box (one)))
    (def x (b --> set (fourtytwo)))
    (b --> get ()))