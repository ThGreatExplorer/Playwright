((module mOne
   (class this (this)
     (method this () 1.0)))
 (module mTwo
   (import mOne)
   (class this (this)
     (method this () 1.0)))
 (import mOne)
 (import mTwo)
 (def that 1.0)
 (def this (new this (that)))
 (def thisOfThis this)
 (thisOfThis isa this))