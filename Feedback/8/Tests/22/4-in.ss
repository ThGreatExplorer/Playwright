((module K
  (class C ()
    (method m (x o) (o --> isaD (x)))))

(module K
  (class D ()
    (method m (x o) (o --> isaC (x)))))

(import K)
(import L)

(def c (new C()))
(def d (new D()))
(c --> m(d cd)))