((module K
  (class c ()
    (method m (x o) (o --> isaD (x)))))

(module L
  (class d ()
    (method m (x o) (o --> isaC (x)))))

(module M
    (import K)
    (import L)
    (class cd (x y)
        (method isaC (x) (x isa c))
        (method isaD (x) (x isa c))))

(import K)
(import L)
(import M)

(def c (new c()))
(def d (new d()))
(def x 1.0)
(def cd (new cd(c d)))
cd)