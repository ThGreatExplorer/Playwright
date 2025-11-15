(
       (module faker
                  (class Faker ()
                        (method fake () (def a (new Faker ()))  (def b (new Faker ()))  (a + b))
                  )
       )

       (timport faker  ( () ( (fake () Number) ) ) )
       (def o (new Faker ()))
       o)
