((class MyClass
        (fieldA fieldB fieldC)
        (method add
                (x y)
                (def a 0.0)
                (def b 0.0)
                (this --> fieldA = x)
                (this --> fieldB = y)
                (a = (this --> fieldA))
                (b = (this --> fieldB))
                (this --> fieldC = (a + b))
                (this --> fieldC)
        )
 )
 (def a 1.0)
 (def test 0.0)
 (test = (a --> fieldZ))
 test
)