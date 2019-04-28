#lang racket

(require (for-syntax syntax/parse)
         math/base)

(provide stx-env-ref)

(define-syntax (stx-env-ref stx)
  (syntax-parse stx
    [(_ (~literal ^) n) #`(^-op n)]
    [(_ (~literal ^) n1 n2) #`(^-op n1 n2)]))

(define-syntax (^-op stx)
  (syntax-parse stx
    [(_ v1 v2) #`(expt v1 v2)]
    [(_ v) #`(exp v)]))