(assert! (meeting accounting (monday 9)))
(assert! (meeting administration (monday 10)))
(assert! (meeting computer (wednesday 15)))
(assert! (meeting administration (friday 13)))
(assert! (meeting the-whole-company (wednesday 16)))

;; a
(meeting ?x (friday ?t))

;; b
(assert! (rule (meeting-time ?person ?day-and-time)
               (or (meeting the-whole-company ?day-and-time)
                   (and (job ?person (?dept . ?rest))
                        (meeting ?dept ?day-and-time)))))

;; c
(meeting-time (Hacker Alyssa P) (wednesday ?time))
