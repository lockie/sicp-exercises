#lang sicp


(define (or-gate o1 o2 output)
    (define (or-action-procedure)
        (let ((new-value
               (logical-or (get-signal o1) (get-signal o2))))
            (after-delay or-gate-delay
                         (lambda ()
                             (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure))

(define (logical-or a b)
    (cond ((or (= a 1) (= b 1)) 1)
          ((and (= a 0) (= b 0)) 0)
          (else (error "Неправильный сигнал" a b))))
