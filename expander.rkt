#lang type-expander

(provide (rename-out [module-begin/j #%module-begin])
         #%app
         #%datum
         #%top)

(define-syntax (module-begin/j stx)
  (syntax-case stx ()
    [(_ a) #`(#%module-begin 2)]))

#;(define-for-syntax (interpret args)
  (first (for/fold ([stack empty])
                   ([arg args]
             #:unless (void? arg))
           (cond
             [(equal? '(0) arg) (cons 0 stack)]
             [(equal? '(1) arg) (cons 1 stack)]
             [else (error "not the droid you're looking for.")]))))