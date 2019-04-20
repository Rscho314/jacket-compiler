#lang racket

(require (prefix-in tr: (only-in typed/racket
                                  #%module-begin
                                  #%top-interaction
                                  #%datum))
         (for-syntax syntax/parse
                     racket))

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction]
                     [datum/j #%datum]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
 (define-syntax-class noun/j
  [pattern n:number #:attr expansion #'n]))

(define-for-syntax (interpret-for-syntax stx stack)
  (define (rec stx stack)
    (let* ([dat (syntax->datum stx)]
           [new-stack (cons dat stack)])
      (displayln new-stack)
      new-stack))
  (rec stx stack))

(define-syntax (expand/j stx)
   (syntax-parse stx
    [(_ s (e:noun/j)) #`#,(first (interpret-for-syntax #`e (syntax->datum #`s)))]
    [(_ s (e:noun/j rest ...+)) (let ([new-stack (interpret-for-syntax #`e (syntax->datum #`s))]) #`(expand/j #,new-stack (rest ...)))]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ ()) #`(tr:#%module-begin)]
    [(_ es) #`(tr:#%module-begin
                     (require "lib.rkt")
                     (expand/j () es))])) ;this does in fact pass a single syntax fragment

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))

(define-syntax (datum/j stx)
  (syntax-case stx ()
    [(_ . a) #`(quote a)])) ;needs additional guard against passing keywords