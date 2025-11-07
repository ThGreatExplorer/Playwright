((tmodule
  Counter
  (class Counter (count) (method getCount () (this --> count)))
  (((count Number)) ((getCount () Number))))
 (import Counter)
 (def u 3.0)
 (def w 42.0)
 (def v w)
 (def d (new Counter (w)))
 (while0 (w == v) (block (def c (new Counter (u))) (v = (w + v)) (d = c)))
 (d --> getCount ()))
