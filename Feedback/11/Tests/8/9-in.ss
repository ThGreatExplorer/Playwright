(
    (module One (class A ()))
    (module Two (class A ()))
    (tmodule Three (timport One (() ())) (class A ()) (() ()))
    (tmodule Four (import Three) (timport One (() ())) (timport Two (() ())) (class B ()) (() ()))
    (module Five (import One) (import Two) (import Four) (class C ()))
    (tmodule Six (timport One (() ())) (timport Two (() ())) (import Three) (import Four) (timport Five (() ())) (class D ()) (() ()))
    (timport One (() ()))
    (timport Two (() ()))
    (import Four)
    (timport Five (() ()))
    1.0
)