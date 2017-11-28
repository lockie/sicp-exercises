#lang sicp


(define (has-cycle x)
    (define (iter x cont elem num)
        (cond ((null? (cdr x)) false)
              ((eq? x elem) true)
              (else (if (= cont num)
                        (iter (cdr x) 0 x (+ 1 num))
                        (iter (cdr x) (+ cont 1) elem num)))))
    (iter x 0 nil 0))


(has-cycle (list 1 2 3))

(define t (list 1 2))
(has-cycle (cons t t))

(define a (list 1 2 3))
(set-cdr! (cddr a) a)
(has-cycle a)
