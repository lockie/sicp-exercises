#lang racket


(define (call-the-cops)
    (display "NOBODY EXPECTS THE SPANISH INQUISITION") (newline))

(define (make-account balance password)
    (define wrong-passwords 0)
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
            (begin (set! wrong-passwords 0)
                   (cond ((eq? m 'withdraw) withdraw)
                         ((eq? m 'deposit) deposit)
                         (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
                                      m))))
            (lambda (x) (begin (set! wrong-passwords (+ 1 wrong-passwords))
                               (cond ((> wrong-passwords 7) (call-the-cops)))
                               "Неверный пароль"))))
    dispatch)

(define acc (make-account 100 'secret-password))

((acc 'some-other-password 'withdraw) 50)
((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)

((acc 'some-other-password 'withdraw) 50)
