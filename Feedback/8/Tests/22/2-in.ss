((module K
  (class C ()
    (method m (x o) (o --> isaD (x)))))

(module L
  (class D ()
    (method m (x o) (o --> isaC (x)))))

(module M
    (import K)
    (import L)
    (class CD ()
        (method isaC (x) (x isa C))
        (method isaD (x) (x isa D))))

(def c (new C()))
(def d (new D()))
(def cd (new CD()))
(c --> m(d cd)))