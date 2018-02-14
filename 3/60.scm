#lang sicp


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

(define (display-stream s)
    (define (display-line x)
        (newline)
        (display x))
    (stream-for-each display-line s))

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


;; actual code

(define (integrate-series S)
    (define (integrate-series-iter S n)
        (cons-stream
         (/ (stream-car S) (stream-car n))
         (integrate-series-iter (stream-cdr S) (stream-cdr n))))
    (integrate-series-iter S integers))

(define cosine-series
    (cons-stream 1
                 (scale-stream
                  (integrate-series sine-series)
                  -1)))

(define sine-series
    (cons-stream 0 (integrate-series cosine-series)))

(define add-series add-streams)

(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1) (stream-car s2))
                 (add-streams (scale-stream
                               (stream-cdr s2)
                               (stream-car s1))
                              (mul-series (stream-cdr s1)
                                          s2))))

(define one (add-series
             (mul-series cosine-series cosine-series)
             (mul-series sine-series sine-series)))

(stream-ref one 0)
(stream-ref one 1)
(stream-ref one 2)
(stream-ref one 3)
(stream-ref one 4)
(stream-ref one 5)
(stream-ref one 6)
(stream-ref one 7)
(stream-ref one 8)
(stream-ref one 9)
(stream-ref one 10)
(stream-ref one 11)
