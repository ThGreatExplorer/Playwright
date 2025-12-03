((tmodule ModuleTypedA
   (class ClassTypedA ()
     (method methodA (paramA) (paramA + paramA)))
   (() ((methodA (Number) Number))))
 (module ModuleBridge
   (import ModuleTypedA)
   (class ClassBridge ()
     (method methodA ()
       (def helper (new ClassTypedA ()))
       (helper --> methodA (this)))))
 (timport ModuleBridge
   (() ((methodA () Number))))
 (def main (new ClassBridge ()))
 (main --> methodA ()))
