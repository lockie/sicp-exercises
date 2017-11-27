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
    (define (dispatch password pw m)
        (if (eq? pw password)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'make-joint)
                   (lambda (new-password) (lambda (pw m) (dispatch new-password pw m))))
                  (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
                               m)))
            (lambda (x) "Неверный пароль")))
    (lambda (pw m) (dispatch password pw m)))

(define (make-joint account password new-password)
    ((account password 'make-joint) new-password))


(define peter-acc (make-account 200 'open-sesame))

(define paul-acc
    (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 40)

((peter-acc 'some-other-password 'deposit) 50)

((paul-acc 'rosebud 'withdraw) 50)

((paul-acc 'wut 'withdraw) 100)
