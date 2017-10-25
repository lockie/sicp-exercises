#lang sicp

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x)
                 x)))

(define (improve y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 8)
