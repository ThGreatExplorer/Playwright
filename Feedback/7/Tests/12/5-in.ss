((class Test (x) (method getX () (this --> x))) 
    (def x 1.0)
    (def test (new Test (x)))
    (test --> getX (x)))