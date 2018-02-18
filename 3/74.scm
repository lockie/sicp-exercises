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

(define sense-data
    (cons-stream
     1 (cons-stream
        2 (cons-stream
           1.5 (cons-stream
                1 (cons-stream
                   0.5 (cons-stream
                        -0.1 (cons-stream
                              -2 (cons-stream
                                  -3 (cons-stream
                                      -2 (cons-stream
                                          -0.5 (cons-stream
                                                0.2 (cons-stream
                                                     3 (cons-stream
                                                        4 nil))))))))))))))

(define (sign-change-detector b a)
    (cond ((and (< a 0) (> b 0)) 1)
          ((and (> a 0) (< b 0)) -1)
          (else 0)))

;; (define (make-zero-crossings input-stream last-value)
;;     (cons-stream
;;      (sign-change-detector (stream-car input-stream) last-value)
;;      (make-zero-crossings (stream-cdr input-stream)
;;                           (stream-car input-stream))))
;; (define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
    (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(display-stream-partial zero-crossings 13)
