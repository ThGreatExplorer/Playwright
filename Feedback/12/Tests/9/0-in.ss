((module Log
  (class Log
    (x)
    (method get ()
      (def tmp (this --> x))
      tmp)))
 (tmodule
  UseLog
  (timport Log (((x Number)) ((get () Number))))
  (class UseLog
    (dummy)
    (method fetch ()
      (def d (this --> dummy))
      (def l (new Log (d)))
      (l --> get ())))
  (((dummy Number)) ((fetch () Number))))
 (module UseTyped
  (import UseLog)
  (class UseTyped
    (x)
    (method run ()
      (def v (this --> x))
      (def u (new UseLog (v)))
      (u --> fetch ()))))
 (timport UseTyped (((x Number)) ((run () Number))))
 (def start 7.0)
 (def m (new UseTyped (start)))
 (m --> run ()))