(
    (tmodule Add 
    (class Adder () (method AddOne (a) (def one 1.0) (a + one))) 
    (() ((AddOne (Number) Number))))
    (tmodule Add 
    (class Adder () (method AddTwo (a) (def two 2.0) (a + two))) 
    (() ((AddTwo (Number) Number))))
    (import Add)
    (def zero 0.0)
    (def adder (new Adder ()))
    (adder --> AddOne(zero))
)