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

;; SYNTAX INTERPRETER DATASTRUCTURES
(begin-for-syntax
  ; The syntax interpreter uses syntax classes matching parts-of-speech
  ; but the lib uses syntax classes matching racket datastructures that
  ; correspond (ensuring semantic equivalence between the two).

  (define-literal-set parts-of-speech
    ; abstraction over syntax-parse options
    #:for-syntax #:datum-literals (§ noun verb) ())
  
  (define-syntax-class noun/j
    ; single-element array reduces to a scalar
    [pattern (n:number) #:attr expansion #`n]
    ; gotcha: data in array constructor syntax
    ; is normally implicitely quoted, but here we need explicit quoting!
    [pattern (n1:number n2:number ...+) #:attr expansion #`(array #['n1 'n2 ...])])

  (define-syntax-class verb/j
    [pattern (~datum ^)])

  (define-syntax-class pos/j
    [pattern e:noun/j
             #:attr pos #'noun]
    [pattern e:verb/j
             #:attr pos #'verb]))

;; SYNTAX INTERPRETER PROCEDURES
; Those patterns are wrong/incomplete (also in the interpret-syntax-fragment).
; The stack size can in fact be >4.
(define-syntax (monadic-pattern-zero/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech)
    [(_ (v:verb/j n:noun/j (~optional (~rest _)))
        (§ verb noun (~optional (~or* noun verb))) e) ; literal noun & verb case
     #`(interpret-syntax-fragment/j
        (#,(local-expand #`(stx-env-ref/j v n.expansion) 'expression #f))
        (§ noun) e)]))

(define-syntax (dyadic-pattern-two/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech)
    [(_ (n1:noun/j v:verb/j n2:noun/j) (§ noun verb noun) e) ; literals
     #`(interpret-syntax-fragment/j
        (#,(local-expand #`(stx-env-ref/j v n1.expansion n2.expansion)
                         'expression #f)) (§ noun) e)]))

(define-syntax (interpret-syntax-fragment/j stx)
  ; Maintain a manual pos-stack: part-of-speech cannot be made a
  ; type because types are unavailable before the end of expansion.
  ; macros that handle executable patterns
  ; 1- fetch the relevant bindings in the environment
  ; 2- use bindings to write the code and keep the result on the value stack
  ; 3- compute the part-of-speech of the resulting fragment and adjust pos-stack
  ; 4- recur to seek another executable pattern
  (syntax-parse stx
    #:literal-sets (parts-of-speech)
    ; monadic application (pattern 0)
    [(_ vs (~and (~var ps) (§ verb noun (~optional (~or* noun verb)))) e)
     #`(monadic-pattern-zero/j vs ps e)]
    ; dyadic application (pattern 2)
    [(_ vs (~and (~var ps) (§ noun verb noun)) e)
     #`(dyadic-pattern-two/j vs ps e)]
    ;  termination condition (single value for now)
    [(_ (v) (§ (~or* noun verb)) ()) #`v] ; (non-)literal value (noun, verb, etc.)
    ; end-of-line encounter (§)
    [(_ (vs ...) (ps ...) (§ ~rest r))
     #`(interpret-syntax-fragment/j (vs ...) (§ ps ...) r)]
    ; part-of-speech encounter
    [(_ (vs ...) (ps ...) (e:pos/j ~rest r))
     #`(interpret-syntax-fragment/j (e vs ...) (e.pos ps ...) r)]
    ; debug
    [(_ vs ps e)
     #`(raise-syntax-error 'interpret-syntax-fragment/j
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
