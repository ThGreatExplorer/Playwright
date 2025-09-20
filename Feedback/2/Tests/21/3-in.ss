(
  (title = 1.0)
  (counter = 10.0)
  (minusOne = -1.0)
  (keepGoing = 0.0)
  (while0 keepGoing
      (block
         (title = (title + title))
         (counter = (counter + minusOne))
         (if0 counter
            (keepGoing = 1.0)
            (keepGoing = 0.0))))
  (offset = 1.0)
  (tilte = (title + offset))
  title
)