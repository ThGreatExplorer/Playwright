((module
  A
  (class A (x)
    (method getX () (this --> x))))
 (module
  B
  (class B (y)
    (method getY () (this --> y))))
 (timport A (((x Number)) ((getX () Number))))
 (timport B (((y Number)) ((getY () Number))))
 (def one 1.0)
 (def two 2.0)
 (def a (new A (one)))
 (def b (new B (two)))
 (def ax (a --> getX ()))
 (def by (b --> getY ()))
 (ax + by))