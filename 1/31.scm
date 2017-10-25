#lang sicp


;(define (product term a next b)
;    (if (> a b)
;        1
;        (* (term a)
;           (product term (next a) next b))))

(define (product term a next b)
    (define (product-iter a result)
        (if (> a b)
            result
            (product-iter (next a) (* result (term a)))))
    (product-iter a 1))


(define (inc n) (+ n 1))

(define (factorial n)
    (product identity 1 inc n))

(factorial 10)


(define (pi terms)
    (define (term n)
        (/
         (+ 2 (* 2 (floor (/ n 2))))
         (+ 1 (* 2 (floor (/ (+ n 1) 2))))))
    (* 4. (product term 1 inc terms)))


(pi 10000)
