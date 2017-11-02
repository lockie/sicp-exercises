#lang sicp


(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (cons x set))

(define (union-set set1 set2)
    (append set1 set2))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))


(union-set (list 1 2 3) (list 3 4 5))
(intersection-set (list 1 2 3) (list 3 4 5))
