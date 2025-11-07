(
  (tmodule DUP
    (class C ()
      ;; Duplicate parameter names: v and v
      (method m (v v) v))
    ;; Shape matches the method arity/types; no other traps
    (() ((m (Number Number) Number))))
  0.0
)
