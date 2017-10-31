#lang sicp


;; fold boilerplate
(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
              (accumulate proc initial (cdr sequence)))))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(define (fold-right op initial sequence)
    (accumulate op initial sequence))


;; actual code


(define (reverse-r sequence)
    (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-l sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse-l (list 1 2 3 4))
(reverse-r (list 1 2 3 4))

