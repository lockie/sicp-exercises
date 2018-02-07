#lang sicp


;; God bless you Danny
(#%require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

;; boilerplate
(define (make-mutex)
    (let ((cell (list false)))
        (define (the-mutex m)
            (cond ((eq? m 'acquire)
                   (if (test-and-set! cell)
                       (the-mutex 'acquire)))
                  ((eq? m 'release) (clear! cell))))
        the-mutex))


;; actual code

(define (make-semaphore n)
    ; some lousy sort of condition variable
    (let ((counter 0)
          (counter-mutex (make-mutex)))
        (define (the-semaphore m)
            (cond ((eq? m 'acquire)
                   (begin
                       (counter-mutex 'acquire)
                       (let ((need-wait (> counter n)))
                           (counter-mutex 'release)
                           (if need-wait
                               (the-semaphore 'acquire))
                           (counter-mutex 'acquire)
                           (set! counter (inc counter))
                           (counter-mutex 'release))))
                  ((eq? m 'release)
                   (begin
                       (counter-mutex 'acquire)
                       (set! counter (dec counter))
                       (counter-mutex 'release)))))
            the-semaphore))
