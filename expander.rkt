#lang turnstile

(require turnstile/no-unicode)

(begin-for-syntax
  (define-syntax-class int
  (pattern a:integer)))

(begin-for-syntax
  (define-syntax-class vecint
  (pattern (a:integer ...))))

(define-base-types VecInt Int)

(define-type-constructor -> #:arity > 0)

(define-primop + : (-> VecInt VecInt VecInt))

;; DATUM
(define-typed-syntax #%datum
  [(_ . (i:int ...+)) >>
   --------
   [/- (#%datum- . 'a) => VecInt]]
  [(_ . i:integer) >>
   --------
   [/- (#%datum- . 'b) => Int]]
  [(_ . x) >>
   --------
   [#:error (type-error #:src #'x
                        #:msg "Unsupported literal: ~v" #'x)]])


#;(begin-for-syntax
  (syntax-parse #'(1 1)
    [v:vecint
     (display (attribute v.vecint))]))