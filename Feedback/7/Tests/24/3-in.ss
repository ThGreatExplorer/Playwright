((class Dog ())
  (class Cat ())

  (def d (new Dog ()))
  (def result 0.0)

  (if0 (d isa Cat)
    (block
      (result = 100.0))
    (block
      (result = 200.0)))
  result)
