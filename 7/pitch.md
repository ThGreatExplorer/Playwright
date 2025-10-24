7/Tests/6-in.ss
^ Path to inegration test exposing the difference between WRONG PITCH and PITCH

## Edited Programs
```
((class While (count)
   (method w()
     (def d (this --> count))
     (def one -1.0)
     (def e (d + one))

     (def result e)
     (if0 e
        (result = e)
        (block
            (this --> count = e)
            (result = (this --> w ()))
        ))
     result))

 (def u 1000.0)
 (def w (new While (u)))
 (w --> w ()))
 ```
Trace tail:
"
69
68
68
68
67
66
66
66
65
64
64
64
63
62
62
62
61
60
60
60
59
58
58
58
57
56
56
56
55
54
54
54
53
52
52
52
51
50
50
50
49
48
48
48
47
46
46
46
45
44
44
44
43
42
42
42
41
40
40
40
39
38
38
38
37
36
36
36
35
34
34
34
33
32
32
32
31
30
30
30
29
28
28
28
27
26
26
26
25
24
24
24
23
22
22
22
21
20
20
20
19
18
18
18
17
16
16
16
15
14
14
14
13
12
12
12
11
10
10
10
9
8
8
8
7
6
6
6
5
4
4
4
3
2
2
2
1
0
"


```
((class While ()
   (method w(other)
     (other --> w (this))))

 (class Repeat (limit)
   (method w(other)
     (def result 0.0)
     (def one -1.0)
     (def curLimit (this --> limit))
     (if0 curLimit
        (result = curLimit)
        (block
            (this --> limit = (curLimit + one))
            (result = (other --> w (this)))))
            
    result))


 (def u 1000.0)
 (def r (new Repeat (u)))
 (def w (new While ()))

 (w --> w(r)))
```
Trace tail: 
"
....
76
75
74
74
74
73
72
72
72
71
70
70
70
69
68
68
68
67
66
66
66
65
64
64
64
63
62
62
62
61
60
60
60
59
58
58
58
57
56
56
56
55
54
54
54
53
52
52
52
51
50
50
50
49
48
48
48
47
46
46
46
45
44
44
44
43
42
42
42
41
40
40
40
39
38
38
38
37
36
36
36
35
34
34
34
33
32
32
32
31
30
30
30
29
28
28
28
27
26
26
26
25
24
24
24
23
22
22
22
21
20
20
20
19
18
18
18
17
16
16
16
15
14
14
14
13
12
12
12
11
10
10
10
9
8
8
8
7
6
6
6
5
4
4
4
3
2
2
2
1
0
"

## Instruction for instrumentation
Run `java -jar ./Other/xruninst.jar` from the assignment directory