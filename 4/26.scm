#lang sicp


(define (unless? exp) (tagged-list? exp 'unless))

(define (unless->combination exp)
    (make-if (cadr exp) (cadddr exp) (caddr exp)))
