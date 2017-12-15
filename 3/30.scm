#lang sicp


(define (riple-carry-adder A B S c)
    (define (iter A B S c)
        (cond ((null? A) nil)
              (else (let (c-inner (make-wire))
                        (full-adder (car A) (car B) c (car S) c-inner)
                        (iter (cdr A) (cdr B) (cdr S) c-inner)))))
    (iter A B S c))
