#lang sicp


;(define (accumulate combiner null-value term a next b)
;    (if (> a b)
;        null-value
;        (combiner (term a)
;                  (accumulate combiner null-value term (next a) next b))))


(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))


(define (product term a next b)
    (accumulate * 1 term a next b))

(define (inc n) (+ n 1))

(define (factorial n)
    (product identity 1 inc n))

(factorial 10)
