(
(module m (class c ()))
(module n (import m) (class d () (method mname () (new c ()))))
(module o (import n) (class e () (method create () (new d ()))))

(import o)
(def x (new e ()))
(x --> create ())
)