#lang racket  ;; NOTE : this is required for (define) order to work


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

(define (integral delayed-integrand initial-value dt)
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                     (if (stream-null? integrand)
                         the-empty-stream
                         (integral (stream-cdr integrand)
                                   (+ (* dt (stream-car integrand))
                                      initial-value)
                                   dt)))))


;; actual code

(define (RLC R L C dt)
    (lambda (vc0 il0)
        (define vc (integral (delay dvc) vc0 dt))
        (define il (integral (delay dil) il0 dt))
        (define dvc (scale-stream il (/ -1 C)))
        (define dil (add-streams
                     (scale-stream il (- (/ R L)))
                     (scale-stream vc (/ 1 L))))
        (cons vc il)))

(define circuit (RLC 1 1 0.2 0.1))

(define currents-and-voltages (circuit 10 0))

(display "*** VOLTAGES\n")
(display-stream-partial (car currents-and-voltages) 10)
(newline)
(display "*** CURRENTS\n")
(display-stream-partial (cdr currents-and-voltages) 10)
