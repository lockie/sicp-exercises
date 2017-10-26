#lang sicp


;; boilerplate

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(define (average-damp f)
    (define (average a b)
        (/ (+ a b) 2))
    (lambda (x) (average x (f x))))

(define (repeated f n)
    (define (compose f g)
        (lambda (x) (f (g x))))
    (cond ((= n 0) identity)
          ((= n 1) f)
          (else (compose f (repeated f (- n 1))))))


;; actual code

(define (root n x)
    (fixed-point
     ((repeated average-damp (floor (/ n 2)))
      (lambda (y) (/ x (expt y (- n 1)))))
     1.))


(root 2 4)
(root 3 8)
(root 4 16)
(root 5 32)
(root 6 64)
(root 7 128)
(root 8 256)
(root 9 512)
(root 10 1024)
