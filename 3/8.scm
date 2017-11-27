#lang sicp


(define (make-f)
    (let ((first-arg nil))
        (lambda (arg)
            (if (null? first-arg)
                (begin (set! first-arg arg)
                       0)
                first-arg))))


; emulate evaluation order
(define f (make-f))
(display "left to right: ") (display (+ (f 0) (f 1))) (newline)
(define f2 (make-f))
(display "right to left: ") (display (+ (f2 1) (f2 0))) (newline)
