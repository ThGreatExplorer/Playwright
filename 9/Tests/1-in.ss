(
    (tmodule ModuleAOne (class A () (method onlyAOne () 0.0)) (() ((onlyAOne () Number))))
    (tmodule ModuleATwo (class A ()) (() ()))

    (tmodule ModuleBAOne (import ModuleAOne) (import ModuleATwo) 
        (class B () (method newA () (new A ())))
        (() ((newA () (() ())))))

    (import ModuleAOne)
    (import ModuleBAOne)

    (def b (new B ()))
    (def someA (b --> newA ()))

    (someA --> onlyAOne())
) 
