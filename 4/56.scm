;; a
(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?address))

;; b
(and (salary (Bitdiddle Ben) ?ben-salary)
     (salary ?name ?salary)
     (lisp-value > ?ben-salary ?salary))

;; c
(and (supervisor ?name ?supervisor)
     (not (job ?supervisor (computer . ?job)))
     (job ?supervisor ?x))
