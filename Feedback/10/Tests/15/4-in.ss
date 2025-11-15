((module A (class C ()))
(tmodule M (timport A (() ())) (timport A (((fieldOne Number) (fieldTwo Number)) ())) 
(class C () (method m (x) (x isa C))) (() ()))

(import M)

1.0)