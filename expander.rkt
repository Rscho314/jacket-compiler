#lang racket

(require (combine-in
          (prefix-in tr: (only-in typed/racket
                                  #%module-begin
                                  #%top-interaction
                                  #%datum))
          (except-in typed/racket
                     #%module-begin
                     #%top-interaction
                     #%datum))
         (for-syntax syntax/parse
                     racket))

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction]
                     [datum/j #%datum]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (i:integer ...+)) #`(tr:#%module-begin (ann #(i ...) (Vectorof Integer)))]))

#;(define-syntax (syntax-parse/j stx)
  (syntax-parser
    []))

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))

(define-syntax (datum/j stx)
  (syntax-case stx ()
    [(_ . a) #`(quote a)])) ;needs additional guard against passing keywords

#;(define-for-syntax (interpret args)
  (first (for/fold ([stack empty])
                   ([arg args]
             #:unless (void? arg))
           (cond
             [(equal? '(0) arg) (cons 0 stack)]
             [(equal? '(1) arg) (cons 1 stack)]
             [else (error "not the droid you're looking for.")]))))