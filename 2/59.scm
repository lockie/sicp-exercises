#lang sicp

;; boilerplate

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))


;; actual code

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((element-of-set? (car set1) set2)
           (union-set (cdr set1) set2))
          (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1 2 3) (list 3 4 5))
