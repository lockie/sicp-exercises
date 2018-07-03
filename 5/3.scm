(controller
 (assign g (const 1.0))
 sqrt-iter
 (assign t (op *) (reg g) (reg g))
 (assign t (op -) (reg t) (reg x))
 (assign t (op abs) (reg t))
 (test (op <) (reg t) (const 0.001))
 (branch (label sqrt-done))
 (assign t (op /) (reg x) (reg g))
 (assign t (op +) (reg t) (reg g))
 (assign g (op /) (reg t) (const 2.0))
 (goto (label sqrt-iter))
 sqrt-done)
