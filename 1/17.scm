#lang planet neil/sicp

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (mult a b)
  (if (= b 0)
    0
    (if (even? b)
      (double (mult a (halve b)))
      (+ a (mult a (- b 1)))
    )
  )
)


(mult 0 2)

(mult 5 8)

(mult 1000 1001)