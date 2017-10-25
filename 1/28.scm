#lang sicp


(define (square x) (* x x))

(define (expmod base exp m)
    (define (squaremod-with-check x)
        (define (test-nontrivial-sqrt1 x square)
            (if (and (= square 1)
                     (not (= x 1))
                     (not (= x (- m 1))))
                0
                square))
        (test-nontrivial-sqrt1 x (remainder (square x) m)))
    (cond ((= exp 0) 1)
          ((even? exp)
           (squaremod-with-check (expmod base (/ exp 2) m)))
          (else
           (remainder (* base (expmod base (- exp 1) m))
                      m))))

(define (miller-rabin-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (prime? n times)
    (cond ((= times 0) true)
          ((miller-rabin-test n) (prime? n (- times 1)))
          (else false)))

(define (test n)
    (display n)
    (display " -> ")
    (prime? n 10)
    )


(test 2)
(test 3)
(test 4)
(test 5)
(test 6)
(test 7)
(test 8)
(test 9)
(test 10)
(test 1000003)
(test 1000033)
(test 1000000)

(test 561)  ; Carmichael number
(test 1105) ; Carmichael number
(test 1729) ; Carmichael number
(test 2465) ; Carmichael number
(test 2821) ; Carmichael number
(test 6601) ; Carmichael number
