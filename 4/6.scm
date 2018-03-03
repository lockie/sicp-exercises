#lang sicp


(define (let? exp) (tagged-list? exp 'let))

(define (make-lambda args body) (list 'lambda args body))

(define (let->combination exp)
    (let ((vars (cadr exp)))
        (append (list (make-lambda (map car vars) (cddr exp)))
                (map cadr vars))))
