
(define (require p)
    (if (not p) (amb)))

(define (list-amb lst)
    (if (null? lst)
        (amb)
        (amb (car lst) (list-amb (cdr lst)))))

(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
              (accumulate proc initial (cdr sequence)))))

(define (list-ref lst n)
    (if (= n 0)
        (car lst)
        (list-ref (cdr lst) (- n 1))))

(define (range n)
    (let res ((i 1))
        (if (> i n)
            '()
            (cons i (res (+ i 1))))))

(define (queens board-size)
    (define (threats? q1 q2)
        (or (= (car q1) (car q2))
            (= (cdr q1) (cdr q2))
            (= (abs
                (- (car q1) (car q2)))
               (abs
                (- (cdr q1) (cdr q2))))))
    (define (safe? board)
        (define (safe-iter row)
            (if (= row 0)
                #t
                (and
                 (let ((col (list-ref board (- row 1))))
                     (not (accumulate
                           (lambda (x y) (or x y))
                           #f
                           (map
                            (lambda (c r)
                                (and
                                 (not (= r row))
                                 (threats?
                                  (cons col row)
                                  (cons c r))))
                            board
                            (range (length board))))))
                 (safe-iter (- row 1)))))
        (safe-iter (length board)))
    (define (add-queen queen board)
        (cons queen board))
    (define (add-queens board k)
        (if (> k board-size)
            board
            (let ((queen (list-amb (range board-size))))
                (let ((new-board (add-queen queen board)))
                    (require (safe? new-board))
                    (add-queens new-board (+ k 1))))))
    (add-queens '() 1))

(queens 8)
