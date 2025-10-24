(
    (class c () (method m (p) (p isa ctwo)))
    (class ctwo () (method m (p) (p isa c)))


    (def x (new c ()))
    (def y (new ctwo ()))

    (y --> m (x))
)
