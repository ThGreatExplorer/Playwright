((module mOne
   (class classA (fieldOne)))
 (import mOne)
 (def field 1.0)
 (def a (new classA (field)))
 (a --> this))