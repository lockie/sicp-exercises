#lang sicp


(define (count-pairs x)
    (define (visited-pair? p pairs)
        (cond ((null? pairs) false)
              ((eq? p (car pairs)) true)
              (else (visited-pair? p (cdr pairs)))))
    (let ((visited-pairs '()))
        (define (count-pairs-internal x)
            (if (not (pair? x))
                0
                (let ((visited (visited-pair? x visited-pairs)))
                    (begin (set! visited-pairs (append visited-pairs (list x)))
                           (if visited
                               0
                               (+ (count-pairs-internal (car x))
                                  (count-pairs-internal (cdr x))
                                  1))))))
        (count-pairs-internal x)))

(count-pairs (cons 1 (cons 2 (cons 3 nil))))  ; 3

(define q (cons 1 2))
(count-pairs (cons q (cons q 1)))  ; 4

(define p '(1))
(define pp (cons q q))
(count-pairs (cons pp pp))  ; 7

(define r (list 1 2 3))
(set-cdr! (cddr r) r)
(count-pairs r)  ; infty
