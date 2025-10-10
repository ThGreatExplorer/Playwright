((def eight 8.0) (def x 0.0)
          (while0 x 
             (block (def one 1.0) 
                    (x = (x + one))
                    (if0 x 
                          (eight = 8.0)
                          (block (def seven 7.0) (x = (x + seven))))))
          x)