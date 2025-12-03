((tmodule ModuleTypedA
   (class ClassTypedA ()
     (method methodA () 0.0))
   (() ((methodA () Number))))
 (tmodule ModuleTypedB
   (class ClassTypedB ()
     (method methodA () 0.0))
   (() ((methodA () Number))))
 (import ModuleTypedA)
 (import ModuleTypedB)
 (def main (new ClassTypedA ()))
 (main isa ClassTypedB))
