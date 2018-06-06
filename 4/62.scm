(assert! (rule (last-pair (?pair) (?pair))))
(assert! (rule (last-pair (?u . ?v) (?pair))
               (last-pair ?v (?pair))))

(last-pair (3) ?x)

(last-pair (1 2 3) ?x)

(last-pair (2 ?x) (3))

(last-pair ?x (3))  ;; hangs
