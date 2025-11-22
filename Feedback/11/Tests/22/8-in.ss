((module SoccerGame (class SoccerGame () (method draw () 0.0))) 
 (tmodule
  Cowboy
  (timport SoccerGame (() ()))
  (class Cowboy () (method draw () 1.0))
  (() ((draw () Number))))
 (module Artist
   (import SoccerGame)
   (class Artist () (method draw () 666.0)))
 (import Cowboy)
 (timport SoccerGame  (() ((draw () Number))))
 (timport Artist (() ((draw () Number))))
 (def a (new Artist ()))
 (def c (new Cowboy ()))
 (def what 1.0)
 (def x (new Artist ()))
 (if0 what (x = a) (x = c))
 (x --> draw ()))