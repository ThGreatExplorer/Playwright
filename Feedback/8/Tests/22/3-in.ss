((module K
  (class C ()
    (method m (x o) (o --> isaD (x)))))

(module M
    (import K)
    (import L)
    (class CD ()
        (method isaC (x) (x isa C))
        (method isaD (x) (x isa D))))

(module L
  (class D ()
    (method m (x o) (o --> isaC (x)))))

(import M)
(import K)
(import L)

(def c (new C()))
(def d (new D()))
(def cd (new CD()))
(c --> m(d cd)))