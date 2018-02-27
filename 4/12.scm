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
    (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))

(define (env-scan var vars vals recurse)
    (cond ((null? vars)
           (recurse))
          ((eq? var (car vars))
           vals)
          (else (env-scan var (cdr vars) (cdr vals) recurse))))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (if (eq? env the-empty-environment)
            (error "Несвязанная переменная" var)
            (let ((frame (first-frame env)))
                (car (env-scan var
                               (frame-variables frame)
                               (frame-values frame)
                               (lambda ()
                                   (env-loop (enclosing-environment env))))))))
    (env-loop env))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (if (eq? env the-empty-environment)
            (error "Несвязанная переменная -- SET!" var)
            (let ((frame (first-frame env)))
                (set-car! (env-scan var
                                    (frame-variables frame)
                                    (frame-values frame)
                                    (lambda ()
                                        (env-loop (enclosing-environment env))))
                          val))))
    (env-loop env))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (set-car! (env-scan var
                            (frame-variables frame)
                            (frame-values frame)
                            (lambda ()
                                (add-binding-to-frame! var val frame)
                                (cons 'ok nil)))
                  val)))


(define test-env the-empty-environment)
(set! test-env (extend-environment '(b) '(42) test-env))
(define-variable! 'a 1 test-env)
(lookup-variable-value 'a test-env)
(set-variable-value! 'a 42 test-env)
(lookup-variable-value 'a test-env)
