#lang racket

(require (combine-in (prefix-in tr: (only-in typed/racket
                                  #%module-begin
                                  #%top-interaction
                                  #%datum))
                     (except-in typed/racket
                                  #%module-begin
                                  #%top-interaction
                                  #%datum))
         (for-syntax syntax/parse))

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction]
                     [datum/j #%datum]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(begin-for-syntax
 (define-syntax-class noun/j
  [pattern (n:number ...+ ~rest r) #:attr expansion #`#(n ...)]))

(begin-for-syntax
 (define-syntax-class verb/j
  [pattern (~datum +) #:attr expansion #''+]))

(define-for-syntax (interpret-for-syntax stx stack)
  (define (rec stx stack)
    (let* ([dat (syntax->datum stx)]
           [new-stack (cons dat stack)])
      (displayln new-stack)
      new-stack))
  (rec stx stack))

(define-syntax (expand/j stx)
  (syntax-parse stx
    ;; noun
    [(_ s ((~seq e:number ...+)))
     #`(ann #,(car (interpret-for-syntax #`#(e ...) (syntax->datum #`s))) (Vectorof Number))]
    [(_ s ((~seq e:number ...+) ~rest r))
     (let ([new-stack (interpret-for-syntax #`#(e ...) (syntax->datum #`s))]) #`(expand/j #,new-stack r))]
    ;; verb
    [(_ s (e:verb/j))
     #`#,(car (interpret-for-syntax #`e (syntax->datum #`s)))]
    [(_ s (e:verb/j ~rest r))
     (let ([new-stack (interpret-for-syntax #`e (syntax->datum #`s))]) #`(expand/j #,new-stack r))]))
    

#;(define-syntax (expand/j stx)
  (syntax-parse stx
    #;[(_ s ((~seq e:number ...+) . r))
     #`'(e ...)]))

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