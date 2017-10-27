#lang sicp


(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(cadr (caddr l1))
(caar l2)
(cadadr (cadadr (cadadr l3)))
