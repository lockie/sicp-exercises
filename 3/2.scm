#lang sicp


(define (make-monitored f)
    (define counter 0)
    (define (mf arg)
        (cond ((eq? arg 'how-many-calls?) counter)
              ((eq? arg 'reset-count) (set! counter 0))
              (else (begin (set! counter (+ counter 1))
                           (f arg)))))
    mf)


(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
