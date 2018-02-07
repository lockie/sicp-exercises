#lang sicp

(#%require (all-except (planet dyoo/sicp-concurrency:1:2/sicp-concurrency) cond))


;; boilerplate

(define (make-mutex)
    (let ((cell (#%app make-cell #f)))
        (define (the-mutex m)
            (cond ((#%app eq? m 'acquire)
                   (if (#%app test-and-set! cell)
                       (#%app the-mutex 'acquire)))
                  ((#%app eq? m 'release)
                   (#%app clear! cell))))
        the-mutex))

(define (make-serializer)
    (let ((mutex (#%app make-mutex)))
        (lambda (p)
            (define (serialized-p . args)
                (#%app mutex 'acquire)
                (let ((val (#%app apply p args)))
                    (#%app mutex 'release)
                    val))
            serialized-p)))

(define (make-numbered-account-and-serializer number balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Недостаточно денег на счете"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) balance)
                  ((eq? m 'number) number)
                  ((eq? m 'serializer) balance-serializer)
                  (else (error "Неизвестный запрос -- MAKE-ACCOUNT"
                               m))))
        dispatch))

(define (exchange account1 account2)
    (let ((difference (- (account1 'balance)
                         (account2 'balance))))
        ((account1 'withdraw) difference)
        ((account2 'deposit) difference)))


;; actual code

(define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer))
          (number1 (account1 'number))
          (number2 (account2 'number)))
        (if (> number1 number2)
            ((serializer1 (serializer2 exchange))
             account1
             account2)
            ((serializer2 (serializer1 exchange))
             account1
             account2))))

(let ((a1 (make-numbered-account-and-serializer 1 50))
      (a2 (make-numbered-account-and-serializer 2 100)))
    (serialized-exchange a1 a2)
    (display (a1 'balance))
    (newline)
    (display (a2 'balance)))
