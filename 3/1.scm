#lang sicp

(define (make-accumulator value)
    (lambda (addendum) (begin (set! value (+ value addendum)) value)))


(define A (make-accumulator 5))

(A 10)

(A 10)
