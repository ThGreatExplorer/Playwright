(
  (module Inner
    (class Inner (x y)))
    
  (module Outer (import Inner)
    (class Outer (x y) 
      (method equalsInner () 
        (def xVal (this --> x))
        (def yVal (this --> y))
        (def innerObj (new Inner (xVal yVal))) 
        (this == innerObj))))

  (timport Outer 
    (((x Number) (y Number)) ((equalsInner () Number))))

  (def nOne 413.0)
  (def nTwo 612.0)
  (def p (new Outer (nOne nTwo)))
  (p --> equalsInner ())
)