#lang sicp


(define (equal? a b)
    (cond ((and (symbol? a) (symbol? b) (eq? a b)) true)
          ((and (null? a) (null? b)) true)
          ((and (list? a) (list? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b))) true)
          (else false)))

(equal? 'a 'a)
(equal? 'a 'b)
(equal? '(a b) '(a b))
(equal? '(a b) '(a c))
(equal? '(a b (c d)) '(a b (c d)))
(equal? '(a b (c d)) '(a b (c e)))
