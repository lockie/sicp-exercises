(assert! (rule (independent? ?person)
               (and
                (job ?person (?dept . ?rest))
                (supervisor ?person ?boss)
                (not (job ?boss (?dept . ?rest2))))))
