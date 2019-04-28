#lang racket

(require math/base
         math/array
         (for-syntax syntax/parse))

(provide stx-env-ref/j)

(begin-for-syntax
  (define-syntax-class array/r
    ; not using #:fail-unless (array? (syntax->datum #`p))
    ; because that would eval the array (which can be big)
    [pattern (array:id #[_ ...+])]))

(define-syntax (stx-env-ref/j stx)
  (syntax-parse stx
    #:datum-literals (^)
    [(_ ^ n) #`(^/j n)]
    [(_ ^ n1 n2) #`(^/j n1 n2)]))

(define-syntax (^/j stx)
  (syntax-parse stx
    ; numbers
    [(_ v1:number v2:number) #`(expt 'v1 'v2)]
    [(_ v:number) #`(exp 'v)]
    ; arrays
    [(_ v1:array/r v2:array/r) #`(array-map expt v1 v2)]
    [(_ v:array/r) #`(array-map exp v)]
    ; numbers + arrays : lift number to array when necessary
    [(_ v1:number v2:array/r) #`(array-map expt (array 'v1) v2)]
    [(_ v1:array/r v2:number) #`(array-map expt v1 (array 'v2))]))