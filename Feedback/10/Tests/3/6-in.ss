(
    (tmodule Add 
    (class Adder (a a) (method AddOne (a) (def one 1.0) (a + one))) 
    (((a Number) (b Number)) ((AddOne (Number) Number))))
    (import Add)
    (def zero 0.0)
    (def adder (new Adder ()))
    (adder --> AddOne(zero))
)