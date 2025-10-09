((def n 5.0)
 (def negOne -1.0)
 (def result 1.0)
 (def keepRunningOuter 0.0)
 (while0 keepRunningOuter
   (block
     (def temp 0.0)
     (def runCount n)
     (def keepRunningInner 0.0)
     (while0 keepRunningInner
       (block 
         (temp = (temp + result))
         (runCount = (runCount + negOne))
         (if0 runCount
          (keepRunningInner = 1.0)
          (keepRunningInner = 0.0))
        ))
     (result = temp)
     (n = (n + negOne))
     (if0 n 
        (keepRunningOuter = 1.0)
        (keepRunningOuter = 0.0))
    ))
 result)