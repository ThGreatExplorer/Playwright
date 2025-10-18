(
(class C (f)
    (method m() (this --> f)))

(def one 1.)
(def six 666.)
(def aC (new C (six)))

(if0 one
    (one = one)
    (block 
        (def two 2.0)
        (def oneC (new C(two)))
        (aC = oneC)))
(aC --> f)
)