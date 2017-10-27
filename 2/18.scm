#lang sicp


(define (reverse l)
    (cond ((eq? l nil) nil)
          ((eq? (cdr l) nil) l)
          (else (append (reverse (cdr l)) (list (car l))))))


(list 1 4 9 16 25)
(reverse (list 1 4 9 16 25))
