#lang racket

; For simplicity, everything is an array fo now.
; This will have to change in the future.
; There is no type checking whatsoever here.

(require math/array
         (for-syntax syntax/parse)
         "syntax-classes.rkt")

(provide stx-env-ref/j)

(define-syntax (stx-env-ref/j stx)
  (syntax-parse stx
    #:datum-literals (^ =:)
    ;assignment
    [(_ n =: e:cav/j)
     #`(=:/j n #,(local-expand #`(stx-env-ref/j e) 'expression #f))]  ; literal case
    [(_ n =: e)
     #`(=:/j n e)]
    ;verbs
    [(_ ^ n ...)
     #`(^/j n ...)]))

;IMPLEMENTATIONS
;assignment
(define-syntax (=:/j stx)
  (syntax-parse stx
    [(_ n e)
     #`(begin (define n e))]))

;verbs
(define-syntax (^/j stx)
  (syntax-parse stx
    [(_ v1 v2)
     #`(array-map expt v1 v2)]
    [(_ v)
     #`(array-map exp v)]
    [(_)
     ; cannot guess context -> must compile to case-lambda
     #`(case-lambda
         [(v) (array-map exp v)]
         [(v1 v2) (array-map expt v1 v2)])]))