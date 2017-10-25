#lang sicp

;;;;;;;;;;;;;;;;;;;;
; (modified) prime? boilerplate

(define (square x) (* x x))

(define (next n)
  (if (= n 2)  ; extra if huh
      3
      (+ n 2)
  )
)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; timed-prime-test boilerplate
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime ) start-time))
  )
)

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


;;;;;;;;;;;;;
; actual code

(define (search-for-primes min-n max-n)
  (search-for-primes-iter max-n (if (= 0 (remainder min-n 2)) (+ min-n 1) min-n))
)

(define (search-for-primes-iter max-n cntr)
  (timed-prime-test cntr)
  (if (< cntr max-n)
      (search-for-primes-iter max-n (+ cntr 2))
  )
)

(search-for-primes 1000 1019)

(search-for-primes 10000 10037)

(search-for-primes 1000000 1000037)
