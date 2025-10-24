((class Test (x) (method getX () (this --> x))) 
    (def one 1.0)
    (def instance (new Test (one)))
    (one + instance))