#lang planet neil/sicp

(define (f n)
  (cond ((< n 3) n)
        (else (+ 
                 (f (- n 1))
                 (f (- n 2))
                 (f (- n 3))
              )
        ) 
  )
)

(f 10)

(define (f-iter f1 f2 f3 counter n)
  (cond ((= counter n) f3)
        (else (f-iter f2 f3 (+ f1 f2 f3) (+ counter 1) n))
  )
)

(define (f2 n)
  (cond ((< n 3) n)
        (else (f-iter 1 2 3 3 n))
  ) 
)

(f2 10)
(f2 100)