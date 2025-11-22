(
    (tmodule Add 
    (class Adder () (method AddOne (a a) (def one 1.0) (a + one))) 
    (() ((AddOne (Number Number) Number))))
    (import Add)
    (def zero 0.0)
    (def adder (new Adder ()))
    (adder --> AddOne(zero))
)