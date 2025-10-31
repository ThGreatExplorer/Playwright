((module Tonk 
    (class Knot (s)))
  (import Tonk)

  (def some 413.0)
  (def knotA (new Knot (some)))
  (def knotB (new Knot (some)))
  (def knotBB (new Knot (knotB)))

  (knotA --> s = knotA)
  (knotB --> s = knotB)

  (if0 (knotA == knotBB) 
    (block (some = 612.0)) 
    (some = knotBB))

  some)
