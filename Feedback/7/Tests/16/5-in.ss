(
    (class
        counter
        (value)
        (method
            inc
            (one)
            (def value (this --> value))
            (this --> value = (value + one))
            (this --> value)
        )
    )
    (def one 1.0)
    (def zero 0.0)
    (def c (new counter (zero)))
    (c --> value = (c --> inc (one)))
    (new counter (one))
)