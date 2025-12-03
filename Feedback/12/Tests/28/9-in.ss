((module
   Helper
   (class Helper (x) (method get () (this --> x))))
 (tmodule
   TypedWrong
   (timport Helper (((x Number)) ((get () Number))))
   (class TypedWrong (h) (method badMethod () this))
   (((h (((x Number)) ((get () Number))))) ((badMethod () Number))))
 (import TypedWrong)
 (timport Helper (((x Number)) ((get () Number))))
 (def five 5.0)
 (def h (new Helper (five)))
 (def tw (new TypedWrong (h)))
 (tw --> badMethod ()))