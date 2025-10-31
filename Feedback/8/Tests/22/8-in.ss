((module K
  (class C ()
    (method m (x o) (o --> isaD (x)))))

(module L
  (class D ()
    (method m (x o) (o --> isaC (x)))
    (method m (x o) (o --> isaD (x)))))

(import K)
(import L)

(def c (new C()))
(def d (new D()))

(c --> m(d c)))