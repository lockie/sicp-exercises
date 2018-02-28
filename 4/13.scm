#lang sicp


(define (make-unbound? exp) (tagged-list? exp 'make-unbound!))

(define (make-unbound var env)
    (define (scan vars vals)
        (cond ((null? vars)
               (error "Variable not bound" var))
              ((eq? var (car vars))
               (set-car! vars nil)
               (set-car! vals nil))
              (else (scan (cdr vars) (cdr vals)))))
    (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame))))
