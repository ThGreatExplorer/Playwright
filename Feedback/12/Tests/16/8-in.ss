(
  (module People
    (class Person (age)
      (method birthday ()
        (def one 1.0)
        (def thousand 1000.0)
        (this --> age = (thousand + one))
        (this --> age))))

  (timport People
    (((age Number))
     ((birthday () Number))))

  (def twenty 20.0)
  (def John (new Person (twenty)))
  (def lebron John)

  (lebron --> birthday())
)
