((module A
   (class Alpha ()
     (method id () 1.0)))

 (module B
   (import A)
   (class Beta ()
     (method id () 2.0)
     (method getAlpha () (new Alpha ()))))

 (module C
   (import B)
   (class Gamma ()
     (method id () 3.0)
     (method getBeta () (new Beta ()))))

 (import C)
 (def g (new Gamma ()))
 (def b (g --> getBeta ()))
 (def a (b --> getAlpha ()))
 (a --> id ()))
