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


;; actual code

(define (random-numbers actions seed)
    (let ((action (stream-car actions))
          (rest-actions (stream-cdr actions)))
        (cond ((eq? action 'generate)
                (begin
                    (random-seed (inexact->exact (floor (* 1000 seed))))
                    (let ((random-number (random)))
                        (cons-stream
                         random-number
                         (random-numbers rest-actions random-number)))))
              ((eq? action 'reset)
               (random-numbers
                (stream-cdr rest-actions)
                (stream-car rest-actions)))
              (else (error "Unknown action")))))

(define test-actions
    (cons-stream
     'reset (cons-stream
             100 (cons-stream
                  'generate (cons-stream
                             'generate (cons-stream
                                        'reset (cons-stream
                                                100 (cons-stream
                                                     'generate (cons-stream
                                                                'generate (cons-stream
                                                                           'reset (cons-stream
                                                                                   200 (cons-stream
                                                                                        'generate (cons-stream
                                                                                                   'generate nil)))))))))))))


(define stream (random-numbers test-actions 0))

(display-stream-partial stream 6)
