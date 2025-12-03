(
  (module U
    (class Pair (a b)
      (method sum ()
        (def r (this --> a))
        (def s (this --> b))
        (r + s)
      )
    )
  )

  (tmodule T
    (timport U (((a Number) (b Number)) ((sum () Number))))
    (class Runner ()
      (method doIt ()
        (def one 20.0)
        (def two 22.0)
        (def p (new Pair (one two)))
        (p --> sum ())
      )
    )
    (() ((doIt () Number)))
  )

  (import T)
  (def r (new Runner ()))
  (r --> doIt ())
)
