#lang sicp


;; accumulate boilerplate
(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
              (accumulate proc initial (cdr sequence)))))


;; actual code
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
                0
                coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
