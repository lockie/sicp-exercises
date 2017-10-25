#lang sicp


;;;;;;;;;;;;;;;;;;;;
; prime? boilerplate

(define (square x) (* x x))

(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))


;;;;;;;;;;;;;
; actual code

(define (filtered-accumulate predicate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (if (predicate a) (term a) null-value)))))
    (iter a null-value))


(define (inc n) (+ n 1))

(define (sum-primes a b)
    (filtered-accumulate prime? + 0 identity a inc b))


(sum-primes 100 1000)  ; 75067

;;;;;;;;
; b part

(define (relatively-prime-sum n)
    (define (predicate i)
        (= 1 (gcd i n)))
    (filtered-accumulate predicate * 1 identity 1 inc n))

(relatively-prime-sum 10)  ; 189

