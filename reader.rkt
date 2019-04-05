#lang racket

;; There is no parser bc J can't be parsed
;; Lexing still useful (can group multiple chars in single token)
(require "lex.rkt")

(provide read-syntax)

(define (read-syntax _ port)
  (define token-list (j-lex port))
  (define module-datum `(module j-mod j/expander ,token-list))
  (datum->syntax #f module-datum))
