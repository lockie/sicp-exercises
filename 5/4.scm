;; a
(controller
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (save n)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 after-expt
 (restore n)
 (restore continue)
 (assign val (op *) (reg b) (reg val))
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (goto (reg continue))
 expt-done)


;; b
(controller
 (assign counter (reg n))
 (assign product (const 1))
 expt-iter
 (test (op =) (reg counter) (const 0))
 (branch (label nonzero))
 (assign counter (op -) (reg counter) (const 1))
 (assign product (op *) (reg b) (reg product))
 (goto (label expt-iter))
 nonzero
 expt-done)
