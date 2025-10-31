(
    (module K 
        (class C () (method m (x) (x isa C)))
        (class D () (method m (x) (x isa D))))
    (import K)
    (def c  (new C()))
    (def d  (new D()))
    (c --> m(d))
)