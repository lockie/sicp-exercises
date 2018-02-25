#lang sicp


;; boilerplate

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)


;; actual code

(define (eval exp env)
    ; stub
    (display "Evaluating expression ")
    (display exp)
    (display " with environment ")
    (display env)
    (newline)
    exp)

(define (list-of-values-ltr exps env)
    (if (no-operands? exps)
        '()
        (let ((first-op (eval (first-operand exps) env))
              (rest-ops (list-of-values-ltr (rest-operands exps) env)))
            (cons first-op
                  rest-ops))))

(define (list-of-values-rtl exps env)
    (if (no-operands? exps)
        '()
        (let ((rest-ops (list-of-values-rtl (rest-operands exps) env))
              (first-op (eval (first-operand exps) env)))
            (cons first-op
                  rest-ops))))

(list-of-values-ltr (list 1 2 3 4) nil)
(list-of-values-rtl (list 1 2 3 4) nil)
