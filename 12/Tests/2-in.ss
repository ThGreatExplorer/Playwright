(
  (module Inner
    (class Inner (x y) (method echo (x) x)))
    
  (module Outer (import Inner)
    (class Outer () (method makeInner (val) (new Inner (val val)))))

  (timport Outer 
    (() ((makeInner (Number) (((x Number) (y (() ()))) ((echo (Number) Number)))))))

  (def n 413.0)
  (def outerP (new Outer ()))
  (def innerP (outerP --> makeInner (n)))
  (innerP --> echo (n))
)