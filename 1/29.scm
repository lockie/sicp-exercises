#lang sicp


(define (cube x) (* x x x))

(define (integral f a b n)
    (define (sum term a next b)
        (if (> a b)
            0
            (+ (term a)
               (sum term (next a) next b))))
    (define h (/ (- b a) n))
    (define (step-x x) (+ x (* 2 h)))
    (* (/ h 3.)
       (+
        (f a)
        (* 4 (sum f (+ a h) step-x (- b h)))
        (* 2 (sum f (step-x a) step-x (- b h)))
        (f b))))

;(integral cube 0 1 5)
;(integral cube 0 1 7)
;(integral cube 0 1 8)
;(integral cube 0 1 10)
(integral cube 0 1 100)
(integral cube 0. 1. 1000.)  ; weird but understandable
