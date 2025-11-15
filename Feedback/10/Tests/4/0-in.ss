(   (module a
                  (class A ())
         )

        (module c
            (import a)
             (class C ()  (method m () (new A ())))
         )

        (tmodule b
                    (timport c  ( () ((m () Number)) )  )

            (class B ()

              (method n ()
                          (def ins (new C ()))
                           (ins --> m ())
               )
            )

            ( ()
              (  (n () Number) )
            )
        )

        (import b)
        (def obj (new B ()))
        (obj --> n ())
        )