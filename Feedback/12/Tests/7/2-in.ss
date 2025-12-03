(
    (module A (class a (f)))
    (timport A (((f Number)) ()))
    (def x 1.0)
    (new a (x))
)