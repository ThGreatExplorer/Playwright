(
    (module myClass (class m ()))
    (module obj (import myClass) 
        (class o () (method myMethod () (new m ()))))

    (tmodule insidious 
        (timport obj (() ((myMethod () Number)))) 
        (class A () 
            (method B () 
                (def o (new o ())) 
                (def x (o --> myMethod ())) 
                (def y 1.0) 
                (x + y))
        )        
        (() ((B () Number)))

    )

    (import insidious)
    (def i (new A ()))
    (def num (i --> B ()))
    num
)