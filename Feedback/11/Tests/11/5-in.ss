(
    (module untypedA (class c ()))
    (tmodule typedA (class c ()) (((x Number)) ()))
    (timport typedA (((x Number)) ()))
    (def a 1.0)
    a
)
