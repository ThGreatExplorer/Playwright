((a = 0.0) (b = 1.0) (c = 2.0) (result = 0.0) (ten = 10.0) (twenty = 20.0) (thirty = 30.0) (forty = 40.0)
 (if0 a
   (if0 b
     (result = ten)
     (result = twenty))
   (if0 (b == c)
     (result = thirty)
     (result = forty)))
 result)