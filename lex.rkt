#lang racket

(require parser-tools/lex)

(provide lexer/j
         tok)

(define-empty-tokens tok
  (ZERO
  ONE
  PLUS
  EOF))

(define lexer/j
  (lexer

   [#\0 (token-ZERO)]
   [#\1 (token-ONE)]

   [#\+ (token-PLUS)]
   
   [#\space (lexer/j input-port)]
   [(eof) (token-EOF)])) ;EOL will need to handled as well (end of J sentence)

#;(module+ test
  (require rackunit)

  ;; j-lexer
  (check-equal? (lexer/j (open-input-string "0")) '(vec ZERO))
  (check-equal? (lexer/j (open-input-string "1")) '(vec ONE))
  (check-equal? (lexer/j (open-input-string "+")) 'PLUS)
  (check-equal? (lexer/j (open-input-string " ")) 'EOF)

  ;; j-lex
  (check-equal? (lex/j (open-input-string "+ 1 0"))
                (list 'EOF 'PLUS 'ZERO 'ONE)))