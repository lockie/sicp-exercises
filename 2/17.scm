#lang sicp


(define (last-pair l)
    (cond ((eq? (cdr l) nil) l)
          (else (last-pair (cdr l)))))


(last-pair (list 23 72 149 34))
