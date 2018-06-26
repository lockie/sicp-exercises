(define (simple-flatten stream)
    (stream-map stream-car
                (stream-filter (lambda (s) (not (stream-null? s))) stream)))
