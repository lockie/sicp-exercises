#lang sicp


;; boilerplate
(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
              (accumulate proc initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (prime? n)
    (define (divides? a b)
        (= (remainder b a) 0))
    (define (square x) (* x x))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (next test-divisor)))))
    (define (smallest-divisor n)
        (find-divisor n 2))
    (define (next n)
        (if (= n 2) 3 (+ n 2)))
    (= n (smallest-divisor n)))

; wut
(define (filter predicate l)
    (cond ((null? l) nil)
          (else (let ((head (car l))
                      (rest (cdr l)))
                    (if (predicate head)
                        (cons head (filter predicate rest))
                        (filter predicate rest))))))


;; actual code

(define (unique-pairs n)
    (flatmap (lambda (i)
                 (map (lambda (j) (list i j))
                      (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

;(unique-pairs 3)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
