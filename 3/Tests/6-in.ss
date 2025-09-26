((fstFib = 0.0)
 (sndFib = 1.0)

 (result = 0.0)
 (n = 10.0)
 (prevvFib = fstFib)
 (prevFib = sndFib)

 (negOne = -1.0)
 (runCount = (n + negOne))
 (keepRunning = 0.0)
 (while0 keepRunning
   (block
     (result = (prevFib + prevvFib))
     (prevvFib = prevFib)
     (prevFib = result)
     (runCount = (runCount + thisoneisnotdefined))
     (if0 runCount
        (keepRunning = 1.0)
        (keepRunning = 0.0))
    ))
 result)