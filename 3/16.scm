#lang sicp


(define (count-pairs x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))

(count-pairs (cons 1 (cons 2 (cons 3 nil))))  ; 3

(define q (cons 1 2))
(count-pairs (cons q (cons q 1)))  ; 4

(define p '(1))
(define pp (cons q q))
(count-pairs (cons pp pp))  ; 7

(define r (list 1 2 3))
(set-cdr! (cddr r) r)
;(count-pairs r)  ; infty
