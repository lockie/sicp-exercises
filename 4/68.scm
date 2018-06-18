(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(assert! (rule (reverse () ())))
(assert! (rule (reverse (?x . ?y) ?z)
               (and
                (reverse ?y ?v)
                (append-to-form ?v (?x) ?z))))
