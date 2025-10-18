((class MyGoodClass
        (fieldA fieldB fieldC)
        (method myGoodMethod
                (paramA paramB)
                (def a paramA)
                (if0 a (a = -1.0)
                       (block (def b paramB)
                              (a = (a + b))
                       )
                )
                a
        )
 )
 (class MyBadClass
        (fieldA fieldB fieldC)
        (method myBadMethod
                ()
                a
        )
        (method myGoodMethod (a b)
                             (a + b)
        )
        (method myBadMethod
                ()
                a
        )
 )
 (def fA 0.0)
 (def fB 1.0)
 (def fC 2.0)
 (def a (new MyGoodClass (fA fB fC)))
 (fA = (a --> fieldB))
 (fB = (a --> fieldA))
 (a --> fieldC = 5.0)
 (a --> myGoodMethod (fA fB))
)