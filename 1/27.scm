#lang sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
           m))))

(define (fermat-test a n)
  (define (try-it x)
    (= (expmod x n n) x))
  (try-it a))

(define (fermat-test-iter n cntr)
  (if (not (fermat-test cntr n))
      (display "RAWR"))
  (if (< cntr (- n 1))
      (fermat-test-iter n (+ cntr 1)))
)

(define (test n)
  (fermat-test-iter n 1))

;(test 560) ; kek

(test 561)
(test 1105)
(test 1729)
(test 2465)
(test 2821)
(test 6601)
