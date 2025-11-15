((tmodule mOne
          (class classA (fieldOne fieldTwo))
          (((fieldOne Number) (fieldTwo Number)) ()))
 (tmodule mOne
          (import mOne)
          (class classA (fieldOne))
          (((fieldOne Number)) ()))
 1.0)