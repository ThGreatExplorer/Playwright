((class MyGoodClass
        (fieldA fieldB fieldC)
        (method myGoodMethod
                (paramA paramB)
                (def fieldA 1.0)
                (def a paramA)
                (this --> fieldA = 1.0)
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
                (dup dup dup dup)
                a
        )
        (method myGoodMethod (a b)
                             (a + b)
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
