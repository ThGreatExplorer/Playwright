((module one (class A ())) (module two (import one) (class B ())) (module three (import one) (import two) (class C ())) (timport three (() ())) 0.1)
