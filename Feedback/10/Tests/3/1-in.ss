(
    (tmodule Add 
    (class Adder () (method AddOne (a) (def one 1.0) (a + one))) 
    (() ((AddOne (Number) Number))))
    (timport Add (() ((AddOne (Number) Number))))
    (def zero 0.0)
    (def adder (new Adder ()))
    (adder --> AddOne(zero))
)