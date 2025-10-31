(
    (module m (class c ()))
    (module n (class c (f)))
    (import m)
    (import n)
    (def one 1.0)
    (def x (new c (one)))
    x
)