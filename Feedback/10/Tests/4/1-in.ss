(   (module a
                  (class A ())
         )

        (module c
            (import a)
             (class C ()  (method m () (new A ())))
         )

        (import b)
        (def obj (new B ()))
        (obj --> n ())
        )