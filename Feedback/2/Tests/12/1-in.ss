(
    (x = -1000.0) 
    (y = 1000.0) 
    (z = (x + y)) 
    (w = (y / y)) 
    (u = (z == w)) 
    (if0 z (block (a = (w + u)) (b = (a / w))) (block (a = -1.0))) 
    (while0 y (block (y = (y + x)))) (final = (a + w)) 
    final)