(
(def title 1.0)
(def counter 10.0)
(def minusOne -1.0)
(def keepGoing 0.0)
(def offset 1.0)
  (while0 keepGoing
      (block
         (title = (title + title))
         (counter = (counter + minusOne))
         (if0 counter
            (keepGoing = 1.0)
            (keepGoing = 0.0))))
  (title = (title + offset))
  title
)