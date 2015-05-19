#lang planet neil/sicp

;;;;;;;;;;;;;;;;;;;;
; fast-prime? boilerplate

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; timed-prime-test boilerplate
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)  ; top kek
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
