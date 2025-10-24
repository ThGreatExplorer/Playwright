(
    (class ListNode (value next)
        (method add (value)
            (def nextNode (this --> next))
            (def ret 0.0)
            (if0 (nextNode isa EmptyListNode)
                (block
                    (def emptyNode (new EmptyListNode ()))
                    (this --> next = (new ListNode (value emptyNode)))
                )
                (ret = (nextNode --> add (value)))
            )
            0.0
        )
        (method get (index)
            (def result 0.0)
            (if0 index
                (result = (this --> value))
                (block
                    (def nextNode (this --> next))
                    (def negOne -1.0)
                    (def nextIndex (index + negOne))
                    (result = (nextNode --> get (nextIndex)))
                )
            )
            result
        )
        (method length ()
            (def one 1.0)
            (def nextNode (this --> next))
            (def result (nextNode --> length ()))
            (result + one)
        )
    )

    (class EmptyListNode ()
        (method initListNode(firstValue)
            (def emptyNode (new EmptyListNode ()))
            (new ListNode (firstValue emptyNode))
        )
        (method length ()
            0.0
        )
    )

    (def ten 10.0)
    (def twenty 20.0)
    (def fourty 40.0)
    (def one 1.0)

    (def myList (new EmptyListNode ()))
    (def ret 0.0)
    (def oneElement 0.0)
    (def length 0.0)

    (myList = (myList --> initListNode (ten)))
    (ret = (myList --> add (twenty)))
    (ret = (myList --> add (fourty)))

    (oneElement = (myList --> get (one)))
    (length = (myList --> length ()))

    (oneElement + length)
)