#lang sicp
(#%require (only racket random-seed random))


;; nasty boilerplate
(#%require racket/stream)
(define-syntax cons-stream
    (syntax-rules ()
        ((_ a b) (stream-cons a b))))
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define stream-null? stream-empty?)
(define the-empty-stream empty-stream)

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred (stream-car stream))
           (cons-stream (stream-car stream)
                        (stream-filter pred
                                       (stream-cdr stream))))
          (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
         low
         (stream-enumerate-interval (+ low 1) high))))

(define (display-line x)
    (newline)
    (display x))

(define (display-stream s)
    (stream-for-each display-line s))

(define (display-stream-partial s N)
    (define (display-stream-partial-iter s n)
        (cond ((< n N)
               (begin
                   (display-line (stream-car s))
                   (display-stream-partial-iter (stream-cdr s) (+ n 1))))
              (else 'done)))
    (display-stream-partial-iter s 0))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
         (apply proc (map stream-car argstreams))
         (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (sqr x) (* x x))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (* range (random)))))


;; actual code

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((experiment)
               (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else
               (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (estimate-integral P x1 y1 x2 y2)
    (define (estimate-integral-iter trials)  ;; XXX not sure if that is what required
        (cons-stream
         (exact->inexact (* (+ (- x2 x1) (- y2 y1))
                            (monte-carlo (stream-car trials)
                                         (lambda () (P (random-in-range x1 x2)
                                                       (random-in-range y1 y2))))))
         (estimate-integral-iter (stream-cdr trials))))
    (estimate-integral-iter (scale-stream integers 100000)))

(define pi (scale-stream
            (estimate-integral
             (lambda (x y) (< (+ (sqr x) (sqr y)) 1))
             0 0 1 1)
            2))

(display-stream pi)
