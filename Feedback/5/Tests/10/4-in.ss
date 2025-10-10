(
    (def i 0.0)
    (while0 i (block
        (def j 1.0)
        (while0 j (block
            (def k 2.0)
            (j = k)
        ))
        (i = (k + k))
    ))
    j
)