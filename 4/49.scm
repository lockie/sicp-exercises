
(define (require p)
    (if (not p) (amb)))

(define (list-amb lst)
    (if (null? lst)
        (amb)
        (amb (car lst) (list-amb (cdr lst)))))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define adjectives '(adj nice warm cool))

(define (parse-sentence)
    (list 'sentence
          (parse-noun-phrase)
          (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
    (list 'simple-noun-phrase
          (parse-word articles)
          (parse-word nouns)))

(define (parse-noun-phrase)
    (define (maybe-extend noun-phrase)
        (amb noun-phrase
             (maybe-extend (list 'noun-phrase
                                 noun-phrase
                                 (parse-prepositional-phrase)))))
    (maybe-extend (parse-simple-noun-phrase)))

(define (parse-prepositional-phrase)
    (list 'prep-phrase
          (parse-word prepositions)
          (parse-noun-phrase)))

(define (parse-verb-phrase)
    (define (maybe-extend verb-phrase)
        (amb verb-phrase
             (maybe-extend (list 'verb-phrase
                                 verb-phrase
                                 (parse-prepositional-phrase)))))
    (maybe-extend (parse-word verbs)))

(define (parse-word word-list)
    (list-amb (cdr word-list)))

(define *unparsed* '())
(define (parse input)
    (set! *unparsed* input)
    (let ((sent (parse-sentence)))
        (require (null? *unparsed*))
        sent))
