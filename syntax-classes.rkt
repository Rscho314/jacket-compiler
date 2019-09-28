#lang racket

; Basically, all "type checking" depends on the definitions in this file

(require (for-syntax syntax/parse)
         math/array)
(provide (for-syntax (all-defined-out)))

(begin-for-syntax
  ; J SYNTAX
  (define-splicing-syntax-class noun/j
    ; for simplicity, everything is lifted to an array for now
    ; gotcha: data in array constructor syntax
    ; is normally implicitely quoted, but here we need explicit quoting!
    [pattern (~seq n:number ...+)
             ; This line expands to the exact same as normal use -> optimal
             #:attr expansion #`(array #(#,@(reverse (syntax->list #'((quote n) ...)))))])

  (define-syntax-class cav/j
    [pattern _:verb/j])
  
  (define-syntax-class verb/j
    [pattern (~datum ^)])

  (define-syntax-class newline-marker/j
    ; abstraction for all syntactic ops
    ; if marker changed in lexer, this is the only change required in expander
    [pattern (~literal §)])
  
  (define-syntax-class name/j
    ; identifiers defined by elimination
    ; means that correctness relies on the lexer
    [pattern (~and (~var n)
                   (~not :number)
                   (~not :verb/j)
                   (~not :newline-marker/j)
                   (~not (~literal =:))
                   (~not (~literal =.))
                   (~not (~literal lparen))
                   (~not (~literal rparen)))])
  
  ; RACKET SYNTAX
  (define-syntax-class array/r
    ; not using #:fail-unless (array? (syntax->datum #`p))
    ; because that would eval the array (which can be big)
    [pattern (array:id #[_ ...+])]))