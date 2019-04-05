#lang racket

(require parser-tools/lex)

(provide j-lex)

;; TODO: structs cannot be introspected at compile time.
;; Maybe turn them into hash tables? (https://stackoverflow.com/a/16380043/2322456)
(define-tokens val (ZERO ONE))
(define-empty-tokens prim (PLUS EOF))

(define j-lexer
  (lexer
   [#\0 (token-ZERO lexeme)]
   [#\1 (token-ONE lexeme)]

   [#\+ (token-PLUS)]

   [" " (j-lexer input-port)]

   [(eof) (token-EOF)])) ;EOL will need to handled as well (end of J sentence)

(define (j-lex ip)
  (define (run acc)
    (let ([tok (j-lexer ip)])
      (if (equal? (token-name tok) 'EOF)
          (cons tok acc)
          (run (cons tok acc)))))
  (run '()))

(module+ test
  (require rackunit)

  ;; j-lexer
  (check-equal? (token-name (j-lexer (open-input-string "0"))) 'ZERO)
  (check-equal? (token-value (j-lexer (open-input-string "0"))) "0")
  (check-equal? (token-name (j-lexer (open-input-string "1"))) 'ONE)
  (check-equal? (token-value (j-lexer (open-input-string "1"))) "1")
  (check-equal? (token-name (j-lexer (open-input-string "+"))) 'PLUS)
  (check-equal? (j-lexer (open-input-string " ")) 'EOF)

  ;; j-lex
  (check-equal? (j-lex (open-input-string "1 0 +"))
                (list 'EOF 'PLUS (token-ZERO "0") (token-ONE "1"))))