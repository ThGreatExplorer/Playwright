(
    (module m (class c (f)))
    (import m)
    (def one 1.0)
    (def x (new c (one)))
    (x --> f = x)
    (x == x)
)