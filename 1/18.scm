#lang planet neil/sicp

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (mult a b)
  (if (= b 0)
    0
    (if (even? b)
      (double (mult a (halve b)))
      (+ a (mult a (- b 1)))
    )
  )
)

(define (fast-mult a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b cntr)
  (if (= cntr b)
    0
    (if (= b 0)
        0
        (if (even? b)
            (double (fast-mult-iter a (halve b) cntr))
            (+ a (fast-mult-iter a b (+ cntr 1)))
        )
    )
  )
)


(fast-mult 0 2)

(fast-mult 5 8)

(fast-mult 1000 1001)