((module
   Widget
   (class Widget (x) (method getX () (this --> x))))
 (module
   Widget
   (class Widget (y) (method getY () (this --> y))))
 (timport Widget (((x Number)) ((getX () Number))))
 (def five 5.0)
 (def w (new Widget (five)))
 (w --> getX ()))
