#lang planet neil/sicp

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (mult a b acc)
  (if (= b 0)
    acc
    (if (even? b)
      (mult (double a) (halve b) acc)
      (mult a (- b 1) (+ acc a))
    )
  )
)


(mult 0 2 0)

(mult 5 8 0)

(mult 1000 1001 0)