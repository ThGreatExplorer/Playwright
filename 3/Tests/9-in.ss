((n = 5.0)
 (negOne = -1.0)
 (result = 1.0)
 (keepRunningOuter = 0.0)
 (while0 keepRunningOuter
   (block
     (temp = 0.0)
     (runCount = n)
     (keepRunningInner = 0.0)
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