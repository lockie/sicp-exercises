(controller
 (assign p (const 1))
 (assign c (const 1))
 factorial-iter
 (test (op >) (reg c) (reg n))
 (branch (label factorial-done))
 (assign p (op *) (reg c) (reg p))
 (assign c (op +) (reg c) (const 1))
 (goto (label factorial-iter))
 factorial-done)
