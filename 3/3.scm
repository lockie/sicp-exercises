#lang racket


(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Недостаточно денег на счете"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch pw m)
        (if (eq? pw password)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
                               m)))
            (lambda (x) "Неверный пароль")))
    dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
