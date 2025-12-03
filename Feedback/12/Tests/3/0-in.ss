(

 (module empty (class Empty ()))

 (module liar (class B (f)))

 (module untyped (import liar) (class C () (method make () (def four 4.0) (new B (four))) ))

 (tmodule typed (timport untyped ( () ( (make ()  ( () () ) ) ) ))

            (class M ()  (method m () (def c (new C ())) (c --> make ())))
            ( () ( (m ()  ( () () ) ) ))
            )

 (tmodule broken (class G (field)) ( ( (field  ( () () ))  ) ()))

 (import typed)
 (import broken)
 (timport empty ( () () ))
 (def hello (new M ()))
 (def lie (hello --> m ()))
 (def empty (new Empty ()))
 (def g (new G (empty)))
 4.0
 )