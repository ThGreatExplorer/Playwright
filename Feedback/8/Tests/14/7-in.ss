((module K (class C () (method m (x o) (o --> isaC(x)))))
 (module L (class C () (method m (x o) (o --> isaC(x)))))
 (module M
    (import L) 
    (class CD ()
    (method isaC (x) (x isa C )))) 
    
(import K)
(import M) 
(def c (new C ()))
(def cd (new CD ())) 
(cd --> isaC(c))) 