#lang sicp


;; accumulate boilerplate
(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
            (accumulate proc initial (cdr sequence)))))


;; actual code

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))


(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (square x) (* x x))

(map square (list 1 2 3))


(define (length sequence)
    (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length (list 1))
(length (list 1 2))
(length (list 1 2 3))
(length (list 1 2 3 4))
(length (list 1 2 3 4 5))
