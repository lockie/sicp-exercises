#lang sicp

(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

(define (cond-proc-clause? clause)
    (eq? (cadr clause) '=>))
(define (cond-proc clause) (caddr clause))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
    (if (null? clauses)
        'false  ; нет ветви else
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (cond ((cond-else-clause? first)
                   (if (null? rest)
                       (sequence->exp (cond-actions first))
                       (error "Ветвь ELSE не последняя -- COND->IF"
                              clauses)))
                  ((cond-proc-clause? first)
                   (make-if (cond-predicate first)
                            (list (cond-proc first) (cond-predicate first))
                            (expand-clauses rest)))
                  (else (make-if (cond-predicate first)
                                 (sequence->exp (cond-actions first))
                                 (expand-clauses rest)))))))
