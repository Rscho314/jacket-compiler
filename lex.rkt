#lang racket
; The lexer unfortunately cannot be discarded, as quote has
; syntactic meaning in scheme.
; TODO: handle parentheses better
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lex/j)

(define-lex-abbrevs
  [name (:: (:+ alphabetic)
            (:* (:or alphabetic numeric))
            (:* (:: (:? #\_) (:+ (:or alphabetic numeric)))))])

(define lexer/j
  (lexer
   ; name
   [name (string->symbol lexeme)]
   ; nouns
   [(:+ numeric) (read (open-input-string lexeme))]

   ;verbs
   [#\^ '^]
   ;adverbs
   [#\/ '/]
   ;assignment
   ["=:" '=:] ;apparently, assignment symbols are treated as single symbols
   ;punctuation
   [#\( 'lparen]
   [#\) 'rparen]
   ;helpers
   [#\space (lexer/j input-port)]
   [#\newline '§]
   [(eof) '()]))

#;(define (lex/j ip)
  (define (run acc)
    (let ([tok (lexer/j ip)])
      (if (equal? tok '())
          acc
          (run (cons tok acc)))))
  (run '(§))) ;start with an start-of-line marker (see J parsing and exec II)

(define (lex/j ip)
  (define (run acc-file acc-line)
    (let ([tok (lexer/j ip)])
      (if (equal? tok '())
          (flatten (reverse (cons acc-line (cons '§ acc-file))))
          (if (equal? tok '§)
           (run (cons acc-line acc-file) (list tok))
           (run acc-file (cons tok acc-line))))))
  (run '() '())) ;start with an start-of-line marker (see J parsing and exec II)

;(lex/j (open-input-string "3"))