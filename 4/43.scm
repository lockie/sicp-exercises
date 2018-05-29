
;; boilerplate
(define (require p)
    (if (not p) (amb)))
(define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))

;; actual code
(define (fathers-daughters)
    (let* ((moore (amb 'maryann))
           (hood (amb 'melissa))
           (downing (amb 'gabriella 'laurna 'rosalind))
           (hall (amb 'gabriella 'laurna 'rosalind))
           (parker (amb 'gabriella 'laurna 'rosalind))
           (names (list 'moore 'downing 'hall 'hood 'parker))
           (daughters (list moore downing hall hood parker))
           (fathers (map cons names daughters)))
        (require
         (distinct? daughters))
        (require
         (not (eq? hood 'gabriella)))
        (require
         (not (eq? moore 'laurna)))
        (require
         (not (eq? hall 'rosalind)))
        (require
         (not (eq? parker 'gabriella)))
        fathers))

(fathers-daughters)
