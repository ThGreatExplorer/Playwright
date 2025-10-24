((class IsaClass ())
(class IsanotherClass ())

(def instance (new IsaClass ()))

(def true (instance isa IsaClass))
(def false (instance isa IsanotherClass))

(true + false))