(
    (module obj (class o ()))
    (module myClass (import obj) (class c () (method one () (new o ()))))
    
    (timport myClass (() ((one () Number))))

    (def a (new c ()))
    (a --> one ())
)