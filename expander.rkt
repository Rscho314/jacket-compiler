#lang racket

(require (combine-in (prefix-in tr: (only-in typed/racket
                                  #%module-begin
                                  #%top-interaction))
                     (except-in typed/racket
                                  #%module-begin
                                  #%top-interaction))
         (for-syntax syntax/parse))

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define-syntax-class noun/j
    [pattern (n:number ...+) #:attr expansion #`#(n ...)]))

(begin-for-syntax
 (define-syntax-class verb/j
  [pattern (~datum +) #:attr expansion #`'+]))

(begin-for-syntax
  (define-syntax-class adverb/j
    [pattern (~literal dot) #:attr expansion #`dot])) ;cannot use "." bc belongs to racket syntax

(begin-for-syntax
 (define-syntax-class conjunction/j
  [pattern (~datum &) #:attr expansion #`'&]))

;; no punctuation syntax class, since "(" and ")" should behave the same as in racket (order of execution?)

(begin-for-syntax
 (define-syntax-class copula/j
  [pattern (~datum =:) #:attr expansion #`'=:]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (interpret-syntax stx stack)
  (define (rec stx stack)
    (let* ([dat (syntax->datum stx)]
           [new-stack (cons dat stack)])
      (displayln new-stack)
      new-stack))
  (rec stx stack))

(define-syntax (expand/j stx)
  (syntax-parse stx
    ;; noun
    [(_ s (e:noun/j))
     #`#,(car (interpret-syntax #`e.expansion (syntax->datum #`s)))]
    [(_ s (e:noun/j ~rest r))
     (let ([new-stack (interpret-syntax #`e.expansion (syntax->datum #`s))]) #`(expand/j #,new-stack r))]
    ;; verb
    [(_ s (e:verb/j))
     #`#,(car (interpret-syntax #`e.expansion (syntax->datum #`s)))]
    [(_ s (e:verb/j ~rest r))
     (let ([new-stack (interpret-syntax #`e.expansion (syntax->datum #`s))]) #`(expand/j #,new-stack r))]))
    

#;(define-syntax (expand/j stx)
  (syntax-parse stx
    [(_ s (e:noun/j))
     #`e.expansion]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ ()) #`(tr:#%module-begin)]
    [(_ es) #`(tr:#%module-begin
                     (require "lib.rkt")
                     (expand/j () es))])) ;this does in fact pass a single syntax fragment

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))
