#lang sicp


;; accumulate boilerplate
(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
              (accumulate proc initial (cdr sequence)))))


;; actual code

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(define (fold-right op initial sequence)
    (accumulate op initial sequence))


(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

; obviously op should be associative
(fold-right * 2 (list 1 2 3))
(fold-left * 2 (list 1 2 3))
