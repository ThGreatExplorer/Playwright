((tmodule Test (class Test (x y)) (((x Number) (y Number)) ()))
 (module TestTwo (class Test ()))
 (tmodule TestThree 
   (timport TestTwo (((x Number)) ()))
   (class Test ()) (() ()))
    (timport TestTwo (((x Number)) ()))
    (def x 1.0)
    (def test (new Test (x)))
    (test isa Test))