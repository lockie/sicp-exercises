#lang sicp


(define (for-each proc l)
    (cond ((null? l) true)
          (else (proc (car l)) (for-each proc (cdr l)))))


(for-each (lambda (x) (display x) (newline))
          (list 57 321 88))
