7/Tests/6-in.ss
^ Path to inegration test exposing the difference between WRONG PITCH and PITCH

## Edited Programs
```
((class While (count)
   (method w()
     (def d (this --> count))
     (def one -1.0)
     (def zero 0.0)
     (def e (d + one))

     (if0 e
        (e = (one / zero))
        (this --> count = e))
     (this --> w ())))

 (def u 1000.0)
 (def w (new While (u)))
 (w --> w ()))
 ```
Trace tail:
"
1
1
0
1
1
1
1
1
1
1
...
"


```
((class While ()
   (method w(other)
     (other --> w (this))))

 (class Repeat (limit)
   (method w(other)
     (def zero 0.0)
     (def one -1.0)
     (def curLimit (this --> limit))
     (if0 curLimit
         (this --> limit = (curLimit + one))
         (zero = (one / zero)))
         
            
    (other --> w (this))))


 (def u 1000.0)
 (def r (new Repeat (u)))
 (def w (new While ()))

 (w --> w(r)))
```
Trace tail: 
"
....
1
1
1
1
1
1
1
1
0
1
0
1
1
1
1
1
1
1
1
1
1
1
1
1
1
...
"

## Instruction for instrumentation
Run `java -jar ./Other/xruninst.jar` from the assignment directory