((module ModuleUntyped
   (class ClassUntyped ()
     (method methodA () 0.0)
     (method methodB () 1.0)))
 (tmodule ModuleClient
   (timport ModuleUntyped
     (() ((methodA () Number))))
   (timport ModuleUntyped
     (() ((methodB () Number))))
   (class ClassClient ()
     (method methodA () 0.0))
   (() ((methodA () Number))))
 0.0)
