((module M (class C (x)))
 (tmodule N
    (timport M)
    (import M)
    (class Ctwo (x))
    (((x Number)) ()))
 (timport M (((x (() ()))) ()))
 (import N)
 0.0
)