(
    (def invaliddecl 0.0)

    (class firstclass (meow bark)
    (method firstmethod (param another param)
    (def getmeow (this --> meow))
    (def getbark (this --> bark))
    (this isa firstclass)))

    (def meowvar 1.0)
    (def barkvar 2.0)
    (def exampleclass (new firstclass (meowvar barkvar)))

    (exampleclass --> meow = 3.0)

    (exampleclass --> meow)
)