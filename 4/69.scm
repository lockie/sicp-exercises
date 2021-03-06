(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (father ?S ?M)
               (or
                (son ?M ?S)
                (and (wife ?M ?W)
                     (son ?W ?S)))))

(assert! (rule (grandson ?G ?S)
               (and (father ?S ?F)
                    (father ?F ?G))))


(assert! (rule (ends-in-grandson (grandson))))
(assert! (rule (ends-in-grandson (?head . ?tail))
               (ends-in-grandson ?tail)))

(assert! (rule ((grandson) ?x ?y)
               (grandson ?x ?y)))

(assert! (rule ((great . ?rel) ?x ?y)
               (and
                (ends-in-grandson ?rel)
                (father ?f ?x)
                (?rel ?f ?y))))


((great grandson) ?g ?ggs)

(?relationship Adam Irad)
