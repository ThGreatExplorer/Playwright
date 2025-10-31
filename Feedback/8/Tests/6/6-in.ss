((module Simple (class Simple
                  ()
                  (method getNumber ()
                    42.0)))
 (import Simple)
 (def s (new Simple ()))
 (s --> getNumber ())
)
