#lang sicp


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Получено слишком много аргументов" vars vals)
            (error "Получено слишком мало аргументов" vars vals))))

(define (make-frame variables values)
    (map cons variables values))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
    (set-cdr! frame (cdr (append frame (list (cons var val))))))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                   (env-loop (enclosing-environment env)))
                  ((eq? var (car vars))
                   (car vals))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Несвязанная переменная" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
    (env-loop env))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan frame)
            (cond ((null? frame)
                   (env-loop (enclosing-environment env)))
                  ((eq? var (caar frame))
                   (set-cdr! (car frame) val))
                  (else (scan (cdr frame)))))
        (if (eq? env the-empty-environment)
            (error "Несвязанная переменная -- SET!" var)
            (let ((frame (first-frame env)))
                (scan frame))))
    (env-loop env))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond ((null? vars)
                   (add-binding-to-frame! var val frame))
                  ((eq? var (car vars))
                   (set-car! vals val))
                  (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
              (frame-values frame))))


(define test-env the-empty-environment)
(set! test-env (extend-environment '(b) '(42) test-env))
(define-variable! 'a 1 test-env)
(lookup-variable-value 'a test-env)
(set-variable-value! 'a 42 test-env)
(lookup-variable-value 'a test-env)
