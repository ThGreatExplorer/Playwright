(
  (class PointA (x y)
    (method addCoord () 2.0)
  )
  (class Multiplier ()
    (method times (m n)
      (def negOne -1.0)
      (def result 0.0) 
      (def keepRunning 0.0)

      (while0 keepRunning
        (block 
          (result = (result + m))
          (n = (n + negOne))
          (if0 n
            (keepRunning = 1.0)
            (keepRunning = 0.0))
          ))
      result)) 

  (def one 1.0)
  (def two 2.0)

  (def pA  (new PointA (one two)))
  (def multer (new Multiplier ()))

  (multer --> times (pA one)))