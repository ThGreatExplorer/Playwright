((tmodule ModuleTypedA
   (class ClassTypedA ()
     (method methodA () 7.0))
   (() ((methodA () Number))))
 (import ModuleTypedA)
 (def main (new ClassTypedA ()))
 (main --> methodA ()))
