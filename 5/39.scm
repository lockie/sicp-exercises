#lang sicp

(define (make-lexical-address frame offset)
    (cons frame offset))

(define (addr-frame addr)
    (car addr))

(define (addr-offset addr)
    (cdr addr))

(define (lexical-address-lookup addr env)
    (let* ((frame (list-ref env (addr-frame addr)))
           (value (list-ref frame (addr-offset addr))))
        (if (eq? value '*unassigned*)
            (error "The variable is unassigned -- LEXICAL-ADDRESS-LOOKUP" addr)
            value)))

(define (lexical-address-set! addr env val)
    (define (set-value! frame offset)
        (if (= offset 0)
            (set-car! frame val)
            (set-value! (cdr frame) (- offset 1))))
    (set-value! (list-ref env (addr-frame addr))
                (addr-offset addr)))
