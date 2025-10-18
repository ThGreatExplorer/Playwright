(
    (class A (A B C)
             (method addition (A B)
                              (B = (A + B))
                              B
             )
    )
    (def A (new A ()))
    (def A (new A (A A A A)))
    (def A (A --> A (A A)))
    A
)
