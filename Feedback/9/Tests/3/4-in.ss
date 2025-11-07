((tmodule module
        (class C () (method mtwo (x y z) (def a x) a))
         ( () ((mtwo (Number Number Number) Number)) ))
          (import module)
                     (def ten 10.0)
                     (def c (new C ()) )
                           (c --> mtwo (ten c ten)))