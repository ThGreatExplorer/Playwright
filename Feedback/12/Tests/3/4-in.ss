(
 (tmodule typed (class Counter (count)) (((count Number)) ()))
 (import typed)
 (def zero 0.0)
 (def c (new Counter (zero)))
 (def one 1.0)
 (def two 2.0)
 (def three 3.0)
 (while0 zero
   (block
   (def curr (c --> count))
   (def done (curr == three))
   (if0 done
     (block (def stop 1.0)
     (zero = stop))
     (block (def next (curr + one))
     (c --> count = next)))))
 (c --> count)
 )