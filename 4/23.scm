;; USAGE:
;; cat 23.scm | time racket 22.scm
;; cat 23.scm | time racket interpeter.scm
(define (fib n)
    (if (< n 3)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))

(fib 35)
(exit)
;; RESULTS
;; racket 22.scm  10,26s user 0,07s system 100% cpu 10,323 total
;; racket interpreter.scm  10,26s user 0,09s system 100% cpu 10,337 total
