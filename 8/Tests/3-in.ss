(
    (module ModuleAOne (class A () (method onlyAOne () 0.0)))
    (module ModuleATwo (class A ()))

    (module ModuleBAOne (import ModuleAOne) (import ModuleATwo) 
        (class B () (method newA () (new A ()))))

    (import ModuleAOne)
    (import ModuleBAOne)

    (def b (new B ()))
    (def someA (b --> newA ()))

    (someA --> onlyAOne())
) 
