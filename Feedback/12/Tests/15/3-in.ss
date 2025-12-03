(
  (tmodule A (class classOne () (method getOne (num) 1.0)) (( ) ((classOne ( ) Number))))
  (tmodule B (import A) 
    (class classOne () 
      (method getTwo () (def one (new classOne ())) (def num 0.0) (def two (one --> getOne (num))) (two / num)))
      (( ) ((getTwo ( ) Number))))
  (import A)
  (import B)
  (def a (new classOne ()))
  (def b (new classOne ()))
  (def num 2.0)
  (num = (b --> getOne (num)))
  a
)