(
    (module A
    (class Knot (val))
)
(import A)
 (def one 1.0)
 (def knt (new Knot (one)))
 (knt --> val = knt)
 (knt == knt))