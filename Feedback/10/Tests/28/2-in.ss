((module
  Data
  (class Storage (val)
    (method get ()
      (this --> val))))
 (tmodule
  User
  (timport Data
    (( (val Number) )
     ( (get () Number) )))
  (class User ()
    (method process (s)
      (def v (s --> get ()))
      (def two 2.0)
      (v + two)))
  (()
   ((process
      ( (( (val Number) )
         ( (get () Number) )) )
      Number)))
  )
 (timport Data
   (( (val Number) )
    ( (get () Number) )))
 (import User)
 (def five 5.0)
 (def store (new Storage (five)))
 (def user (new User ()))
 (def result (user --> process (store)))
 result)