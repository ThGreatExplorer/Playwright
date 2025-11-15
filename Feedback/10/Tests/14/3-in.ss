(
  (tmodule A (class classOne () (method getOne (num) 1.0)) (( ) ((classOne ( ) Number))))
  (module B (import A) 
    (class classOne () 
      (method getTwo () (def one (new classOne ())) (def num 0.0) (def two (one --> getOne (num))) (two / num))))
  (import A)
  (timport B (( ) ((getTwo ( ) Number))))
  (def a (new classOne ()))
  (def b (new classOne ()))
  (def num 2.0)
  (num = (b --> getOne (num)))
  a
)