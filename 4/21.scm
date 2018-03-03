#lang sicp


;; a

((lambda (n)
     ((lambda (fact)
          (fact fact n))
      (lambda (ft k)
          (if (= k 1)
              1
              (* k (ft ft (- k 1)))))))
 10)

((lambda (n)
     ((lambda (fib)
          (fib fib n))
      (lambda (fi k)
          (if (< k 3)
              1
              (+ (fi fi (- k 1))
                 (fi fi (- k 2)))))))
 10)


;; b

(define (f x)
    ((lambda (even? odd?)
         (even? even? odd? x))
     (lambda (ev? od? n)
         (if (= n 0) true (od? ev? od? (- n 1))))
     (lambda (ev? od? n)
         (if (= n 0) false (ev? ev? od? (- n 1))))))
(f 10)
(f 11)
