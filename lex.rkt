#lang racket

(require parser-tools/lex)

(provide lex/j)


;; Use hash tables for introspection at compile time


(define lexer/j
  (lexer
   [#\0 'ZERO]
   [#\1 'ONE]

   [#\+ 'PLUS]

   [" " (lexer/j input-port)]

   [(eof) 'EOF])) ;EOL will need to handled as well (end of J sentence)

(define (lex/j ip)
  (define (run acc)
    (let ([tok (lexer/j ip)])
      (if (equal? tok 'EOF)
          (cons tok acc)
          (run (cons tok acc)))))
  (run '()))

(module+ test
  (require rackunit)

  ;; j-lexer
  (check-equal? (lexer/j (open-input-string "0")) 'ZERO)
  (check-equal? (lexer/j (open-input-string "1")) 'ONE)
  (check-equal? (lexer/j (open-input-string "+")) 'PLUS)
  (check-equal? (lexer/j (open-input-string " ")) 'EOF)

  ;; j-lex
  (check-equal? (lex/j (open-input-string "1 0 +"))
                (list 'EOF 'PLUS 'ZERO 'ONE)))