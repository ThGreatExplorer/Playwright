((module Bank
   (class bank ()
     (method init ()
     (new bank ()))))
 (tmodule typedModule
          (timport Bank (() ((init () Number))))
          (class u ()
            (method initBank ()
                    (def m (new bank ()))
                    (m --> init ())))
          (() ((initBank () Number))))
 (import typedModule)
 (def b (new u ()))
 (b --> initBank ()))