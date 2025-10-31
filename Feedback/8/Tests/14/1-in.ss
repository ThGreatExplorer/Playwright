((module M (class C () (method m (x) (x isa D))))
 (module K (import M) (class D () (method m (x) (x isa C))))

(import M)
(import K)

(def c (new C()))
(def d (new D()))

(c --> m(d)))