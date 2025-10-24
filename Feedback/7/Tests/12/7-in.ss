((class Test (x) (method getX () (this --> x))) 
    (def one 1.0)
    (def instance (new Test (one)))
    (def result -1.0)
    (if0 instance
        (result = 0.0)
        (result = 1.0))
    result)