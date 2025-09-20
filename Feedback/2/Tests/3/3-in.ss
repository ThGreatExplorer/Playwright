(
(x = 0.0)
(x = x)
(x = (x + x))
(x = (x / x))
(x = (x == x))

(if0 0.0 (x = 0.0) (x = 0.0))
(if0 x (x = x) (x = 0.0))
(if0 (x + x) (x = (x + x)) (x = 0.0))
(if0 (x / x) (x = (x == x)) (x = 0.0))
(if0 (x == x) (x = (x / x)) (x = 0.0))

(if0 0.0 (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))
(if0 x (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))
(if0 (x + x) (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))
(if0 (x / x) (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))
(if0 (x == x) (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))

(while0 0.0 (x = 0.0))
(while0 x (x = x))
(while0 (x + x) (x = (x + x)))
(while0 (x / x) (x = (x == x)))
(while0 (x == x) (x = (x / x)))


(while0 0.0 
(block
(x = 0.0)
(x = x)
(x = (x + x))
(x = (x / x))
(x = (x == x))

(if0 0.0 (x = 0.0) (x = 0.0))
(if0 x (x = x) (x = 0.0))
(if0 (x + x) (x = (x + x)) (x = 0.0))
(if0 (x / x) (x = (x == x)) (x = 0.0))
(if0 (x == x) (x = (x / x)) (x = 0.0))

(if0 0.0 (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))
(if0 x (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))
(if0 (x + x) (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))
(if0 (x / x) (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))
(if0 (x == x) (block (x = 0.0) (x = 0.0)) (block (x = 0.0) (x = 0.0)))

(while0 0.0 (x = 0.0))
(while0 x (x = x))
(while0 (x + x) (x = (x + x)))
(while0 (x / x) (x = (x == x)))
(while0 (x == x) (x = (x / x))))
)

(if0 0.0 (if0 0.0 (if0 0.0 (if0 0.0 (x = 0.0) (x = 0.0)) (x = 0.0)) (x = 0.0)) (x = 0.0))
(if0 0.0 (block (if0 0.0 (if0 0.0 (if0 0.0 (if0 0.0 (block (if0 0.0 (block (if0 0.0 (if0 0.0 (if0 0.0 (if0 0.0 (block (x = 0.0) (x = 0.0))  (x = 0.0)) (x = 0.0)) (x = 0.0)) (x = 0.0)) (x = 0.0)) (block (x = 0.0) (x = 0.0))) (x = 0.0))  (if0 = 0.0)) (x = 0.0)) (x = 0.0)) (x = 0.0)) (x = 0.0)) (block (x = 0.0) (x = 0.0)))

x
)
