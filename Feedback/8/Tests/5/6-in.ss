(
    (module n (class c (f)))
    (module m (import n) (class c (f p)))
    (import m)
    (def zero 0.0)
    (def one 1.0)
    (def x (new c (zero one)))
    (x --> p)
)