#lang sicp
(#%require racket/math)


(define (make-from-mag-ang r a)
    (define (dispatch op)
        (cond ((eq? op 'real-part) (* r (cos a)))
              ((eq? op 'imag-part) (* r (sin a)))
              ((eq? op 'magnitude) r)
              ((eq? op 'angle) a)
              (else (error "Unknown operation " op))))
    dispatch)

(define (apply-generic op arg) (arg op))


(define z (make-from-mag-ang 2 (- (/ pi 3))))

(apply-generic 'real-part z)
(apply-generic 'imag-part z) ; -sqrt 3 = -1.732
(apply-generic 'magnitude z)
(apply-generic 'angle z)

