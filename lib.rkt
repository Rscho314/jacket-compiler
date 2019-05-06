#lang racket

; For simplicity, everything is an array fo now.
; This will have to change in the future.
; There is no type checking whatsoever here.

(require math/array
         (for-syntax syntax/parse))

(provide stx-env-ref/j)

(define-syntax (stx-env-ref/j stx)
  (syntax-parse stx
    #:datum-literals (^ =:)
    ;assignment
    [(_ n =: e) #`(=:/j n e)]
    ;verbs
    [(_ ^ n) #`(^/j n)]
    [(_ ^ n1 n2) #`(^/j n1 n2)]))

;IMPLEMENTATIONS
;assignment
(define-syntax (=:/j stx)
  (syntax-parse stx
    [(_ n e) #`(define n e)]))

;verbs
(define-syntax (^/j stx)
  (syntax-parse stx
    [(_ v1 v2) #`(array-map expt v1 v2)]
    [(_ v) #`(array-map exp v)]))