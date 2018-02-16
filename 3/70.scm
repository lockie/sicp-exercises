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

(define (merge-weighted weight s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
               (cond ((< (weight s1car) (weight s2car))
                      (cons-stream s1car (merge-weighted weight (stream-cdr s1) s2)))
                     ((> (weight s1car) (weight s2car))
                      (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2))))
                     (else
                      (cons-stream s1car
                                   (merge-weighted
                                    weight
                                    (stream-cdr s1)
                                    (stream-cdr s2)))))))))

(define (weighted-pairs weight s t)
    (cons-stream
     (list (stream-car s) (stream-car t))
     (merge-weighted
      weight
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

(define S1 (weighted-pairs
            (lambda (pair) (+ (car pair) (cadr pair)))
            integers
            integers))

(display-stream-partial S1 20)

(define S2 (stream-filter
            (lambda (pair)
                (let ((i (car pair)) (j (cadr pair)))
                    (and (not (= 0 (remainder i 2)))
                         (not (= 0 (remainder i 3)))
                         (not (= 0 (remainder i 5)))
                         (not (= 0 (remainder j 2)))
                         (not (= 0 (remainder j 3)))
                         (not (= 0 (remainder j 5))))))
            (weighted-pairs
             (lambda (pair)
                 (let ((i (car pair)) (j (cadr pair)))
                     (+ (* 2 i) (* 3 j) (* 5 i j))))
             integers
             integers)))

(display-stream-partial S2 20)
