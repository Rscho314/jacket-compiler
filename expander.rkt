#| The execution stack won't exist at runtime, so can't use types
   in the expander (hence using #lang typed/racket not allowed.
   So, verification of the well-typedness of the stack at compile
   time forces us to use symbols. However, we then expand to type
   executable fragments with type-expander/lang, which enables a
   certain amount of a posteriori type-checking after expansion.

   Typing:
   - Typing annotations are optional.
   - If absent:
     1. attempt to infer type from source text
     2. default to Any 
   - For now, adopt typed/racket syntax: (ann X (Type))
   - Provide a terser syntax afterwards
   - Can be provided for every symbol

   But first, let's do an untyped version...|#

#lang racket

(require (prefix-in te: typed/racket)
         (for-syntax syntax/parse)
         "lib.rkt"
         math/array)

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define-syntax-class noun/j
    [pattern (n:number ...+)
             ; GOTCHA: data in array constructor syntax
             ; is normally impicitely quoted, so we need explicit quoting!
             #:attr exp #`(array #['n ...])])

  (define-syntax-class verb/j
    [pattern (~datum ^)])

  (define-syntax-class pos/j
    [pattern e:noun/j
             #:attr pos #'noun]
    [pattern e:verb/j
             #:attr pos #'verb]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Proof-of-concept on scalars.
; Those patterns are wrong/incomplete (also in the interpret-syntax-fragment)
(define-syntax (monadic-pattern-zero stx)
  (syntax-parse stx
    #:datum-literals (noun verb §)
    [(_ (v:verb/j (n)) (§ verb noun) e)
     #`(stx-env-ref v 'n)]))

(define-syntax (dyadic-pattern-two stx)
  (syntax-parse stx
    #:datum-literals (noun verb §)
    [(_ ((n1) v:verb/j (n2)) (§ noun verb noun) e)
     #`(stx-env-ref v 'n1 'n2)]))

(define-syntax (interpret-syntax-fragment/j stx)
  ; Maintain a manual pos-stack: part-of-speech cannot be made a
  ; type because types are unavailable before the end of expansion.
  ; macros that handle executable patterns
  ; 1- fetch the relevant bindings in the environment
  ; 2- use bindings to write the code and keep the result on the value stack
  ; 3- compute the part-of-speech of the resulting fragment and adjust pos-stack
  ; 4- recur to seek another executable pattern
  (syntax-parse stx
    #:datum-literals (noun verb §)
    ; monadic application (pattern 0)
    [(_ vs (~and (~var ps) (§ verb noun)) e)
     #`(monadic-pattern-zero vs ps e)]
    ; dyadic application (pattern 2)
    [(_ vs (~and (~var ps) (§ noun verb noun)) e)
     #`(dyadic-pattern-two vs ps e)]
    ;  termination condition (single value for now)
    #;[(_ (v) ((~literal §) (~or noun verb)) ())
     #`'v]
    [(_ (n:noun/j) ((~literal §) noun) ())
     #`n.exp] ; no variable for now, so we expand directly
    [(_ (v:verb/j) ((~literal §) verb) ())
     #`'v]
    ; end-of-line encounter (§)
    [(_ (vs ...) (ps ...) ((~literal §) ~rest r))
     #`(interpret-syntax-fragment/j (vs ...) (§ ps ...) r)]
    ; part-of-speech encounter
    [(_ (vs ...) (ps ...) (e:pos/j ~rest r))
     #`(interpret-syntax-fragment/j (e vs ...) (e.pos ps ...) r)]
    ; debug
    [(_ vs ps e)
     #`(raise-syntax-error 'interpret-syntax-fragment
                           "debug condition"
                           (list 'vs 'ps 'e))]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (§)) #`(#%module-begin)]
    [(_ es) #`(#%module-begin
               (interpret-syntax-fragment/j () () es))]))

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))
