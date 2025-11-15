(
    (tmodule BadEmpty
        (class EmptyListNode ()
            (method length ()
                1.0
            )
        )
        (() ((length () Number)))
    )

    (module Empty
        (import BadEmpty)
        (class EmptyListNode ()
            (method getEmpty ()
                (new EmptyListNode())
            )
            (method length ()
                0.0
            )
        )
    )

    (module Collection
        (import BadEmpty)
        (import Empty)

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

            (method initList (initialValue)
                (def listEnd (new EmptyListNode ()))
                (new ListNode(initialValue listEnd))
            )
        )
    )

    (timport Empty (() ((initList (Number) (() ((add (Number) Number) (get (Number) Number) (length () Number)))))))
    (timport Collection (
        ((a Number) (b Number))
        (
            (add (Number) Number)
            (get (Number) Number)
            (length () Number)
            (initList (Number) (
                ((a Number) (b Number))
                (
                    (add (Number) Number)
                    (get (Number) Number)
                    (length () Number))
                )
            )
        )
    ))

    (def ten 10.0)
    (def twenty 20.0)
    (def fourty 40.0)
    (def one 1.0)

    (def startList (new ListNode (one one)))
    (def myList (startList --> initList (ten)))

    (def ret 0.0)
    (def oneElement 0.0)
    (def length 0.0)

    (ret = (myList --> add (twenty)))
    (ret = (myList --> add (fourty)))

    (oneElement = (myList --> get (one)))
    (length = (myList --> length ()))

    (oneElement + length)
)