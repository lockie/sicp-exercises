#lang sicp


;; get/put boilerplate

(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define *eval-table* (make-hash))

(define (put exptype proc)
    (hash-set! *eval-table* exptype proc))

(define (get exptype)
    (hash-ref *eval-table* exptype '()))


;; some stubs

(define (set-variable-value! var value env)
    'sure)

(define (define-variable! var value env)
    'sure)

(define (make-procedure name body env)
    'sure)

(define (lookup-variable-value exp env)
    +)

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))


;; actual code

(define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))

(define (variable? exp) (symbol? exp))

(put 'qoute (lambda (exp env) (car exp)))

(define (eval-assignment exp env)
    (set-variable-value! (car exp)
                         (eval (cadr exp) env)
                         env)
    'ok)
(put 'set! eval-assignment)

(define (eval-definition exp env)
    (define-variable! (definition-variable exp)
        (eval (definition-value exp) env)
        env)
    'ok)
(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (car exp)
        (caar exp)))
(define (definition-value exp)
    (if (symbol? (car exp))
        (cadr exp)
        (make-lambda (cdar exp)
                     (cdr exp))))
(put 'define eval-definition)

(put 'lambda (lambda (exp env)
                 (make-procedure (car exp)
                                 (cdr exp)
                                 env)))
(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

(define (if-predicate exp) (car exp))
(define (if-consequent exp) (cadr exp))
(define (if-alternative exp)
    (if (not (null? (cddr exp)))
        (caddr exp)
        'false))
(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))
(define (eval-if exp env)
    (if (not (null? (eval (if-predicate exp) env)))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
(put 'if eval-if)

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))
(put 'begin eval-sequence)

(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
    (if (null? clauses)
        'false  ; нет ветви else
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "Ветвь ELSE не последняя -- COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))
(put 'cond (lambda (exp env) (eval (cond->if exp) env)))

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((not (null? (get (car exp)))) ((get (car exp)) (cdr exp) env))
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else
           (error "Неизвестный тип выражения -- EVAL" exp))))


(eval (list 'if 1 (list '+ 1 2)) '())  ;; sweet_mother_of_god.svgz
