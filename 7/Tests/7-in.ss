((class Fib (fstFib sndFib)
  (method calcN (n)
    (def result 0.0)
    (def prevvFib (this --> fstFib))
    (def prevFib  (this --> sndFib))

    (def negOne -1.0)
    (def runCount (n + negOne))
    (def keepRunning 0.0)

    (while0 keepRunning
      (block
        (result = (prevFib + prevvFib))
        (prevvFib = prevFib)
        (prevFib = result)
        (runCount = (runCount + negOne))
        (if0 runCount
            (keepRunning = 1.0)
            (keepRunning = 0.0))
        ))

    result))

  (def fstNum 0.0)
  (def sndNum 1.0)
  (def fibInstance (new Fib (fstNum sndNum)))
  
  (def n 10.0)
  (def result (fstNum --> calcN (n)))

  result)