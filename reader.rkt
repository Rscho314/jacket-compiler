#lang racket

;; There is no parser bc J can't be parsed
;; Lexing still useful (can group multiple chars in single token)
(require "lex.rkt"
         "pre-expander.rkt")

(provide read-syntax)

(define (read-syntax _ port)
  (define semi-ast (lex/j port))
  (define module-datum `(module mod/j typed/racket ,(pre-expand semi-ast)))
  (datum->syntax #f module-datum))

(module+ test
  (require rackunit)
  (check-eqv? (read-syntax "" (open-input-string "1"))
                (datum->syntax
                 #f
                 '())))