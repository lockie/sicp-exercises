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
    x)  ;; NOTE : noop for this excercise

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

(define (square x) (* x x))

(define (merge-weighted weight s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
               ;(newline) (display s1car) (display " ") (display s2car) (display " ")
               ;(display (weight s1car)) (display " ") (display (weight s2car)) (newline)
               (cond ((< (weight s1car) (weight s2car))
                      (cons-stream
                       s1car
                       (merge-weighted weight (stream-cdr s1) s2)))
                     (else
                      (cons-stream
                       s2car
                       (merge-weighted weight s1 (stream-cdr s2)))))))))

(define (weighted-pairs weight s t)
    (cons-stream
     (list (stream-car s) (stream-car t))
     (merge-weighted
      weight
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

(define (weight pair) (+ (square (car pair)) (square (cadr pair))))

(define (filter-twosquare-numbers S)
    (let* ((current (stream-car S))
          (next (stream-car (stream-cdr S)))
          (after-next (stream-car (stream-cdr (stream-cdr S))))
          (current-weight (weight current))
          (next-weight (weight next))
          (after-next-weight (weight after-next)))
        (cond ((and (= current-weight next-weight)
                    (= next-weight after-next-weight))
               (begin
                   (newline)
                   (display current) (display " ")
                   (display next) (display " ")
                   (display after-next) (display " ")
                   (display current-weight)
                   (newline)
                   (cons-stream
                    current
                    (filter-twosquare-numbers (stream-cdr (stream-cdr (stream-cdr S)))))))
              (else (filter-twosquare-numbers (stream-cdr S))))))

(define twosquare-numbers (filter-twosquare-numbers
                           (weighted-pairs weight integers integers)))

(display-stream-partial twosquare-numbers 10)
