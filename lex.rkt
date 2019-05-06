#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lex/j)

;; super chobo
(define (read-vec s)
  (map (λ (t) (read (open-input-string t))) (string-split s)))

(define-lex-abbrevs
  [vecnum (:: (:+ numeric) (:* (:: #\space (:+ numeric))))]
  [name (:: (:+ alphabetic)
            (:* (:or alphabetic numeric))
            (:* (:: (:? #\_) (:+ (:or alphabetic numeric)))))])

(define lexer/j
  (lexer
   ; name
   [name (string->symbol lexeme)]
   ; nouns
   [vecnum (read-vec lexeme)]

   ;verbs
   [#\^ '^]
   ;adverbs

   ;assignment
   ["=:" '=:] ;apparently, assignment symbols are treated as single symbols
   ;punctuation
   [#\( 'lparen]
   [#\) 'rparen]
   ;helpers
   [#\space (lexer/j input-port)]
   [#\newline '§]
   [(eof) '()]))

(define (lex/j ip)
  (define (run acc)
    (let ([tok (lexer/j ip)])
      (if (equal? tok '())
          acc
          (run (cons tok acc)))))
  (run '(§))) ;start with an start-of-line marker (see J parsing and exec II)

(module+ test
  (require rackunit)
  (check-equal? (lex/j (open-input-string "r_34534_thr4_45=:10 2 3")) '()))