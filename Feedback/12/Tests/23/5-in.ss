((module ModuleUntyped
   (class ClassUntyped ()
     (method methodA () ghost)))
 (timport ModuleUntyped
   (() ((methodA () Number))))
 (def main (new ClassUntyped ()))
 (main --> methodA ()))
