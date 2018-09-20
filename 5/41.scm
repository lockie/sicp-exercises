#lang sicp


(define (make-lexical-address frame offset)
    (cons frame offset))

(define (find-variable var env)
    (define (offset el lst)
        (if (eq? el (car lst))
            0
            (+ 1 (offset el (cdr lst)))))
    (define (find-variable-iter var env frame)
        (if (null? env)
            '*not-found*
            (if (memq var (car env))
                (make-lexical-address frame (offset var (car env)))
                (find-variable-iter var (cdr env) (+ 1 frame)))))
    (find-variable-iter var env 0))


;; test
(find-variable 'c '((y z) (a b c d e) (x y)))
(find-variable 'x '((y z) (a b c d e) (x y)))
(find-variable 'w '((y z) (a b c d e) (x y)))
