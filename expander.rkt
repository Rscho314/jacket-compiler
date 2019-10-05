#| Untyped version
   ---------------
   This uses symbols to enforce "well-typedness".
   All "type checking" happens in syntax-classes.rkt.

   TODO
   ----
   + Finish adverb implementation (lib.rkt)
   + Problem: the j embedding macro fails (see main.rkt)
   + Macro stepper internal error: bug in math/array when using untyped racket
     (https://github.com/racket/math/issues/24)
   + How to handle the "anything" pattern in the J parse & execution table?
     - always null?
   + Handle names better (not by elimination as not part-of-speech)|#

#lang racket

(require (prefix-in tr:
                    (only-in typed/racket
                             #%module-begin
                             #%top-interaction))
         (for-syntax syntax/parse
                     "lex.rkt")
         "syntax-classes.rkt"
         "lib.rkt")

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction])
         j)

;; SYNTAX INTERPRETER DATASTRUCTURES
(begin-for-syntax
  (define-literal-set parts-of-speech+names
    #:for-syntax #:datum-literals (newline-marker
                                   =.
                                   =:
                                   lparen
                                   rparen
                                   noun
                                   verb
                                   adverb
                                   conjunction
                                   name) ()))

;; SYNTAX INTERPRETER PROCEDURES
(define-syntax (monadic-zero/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ prs
        (v expr1 expr2 expr3 ...)
        (pv _ _ pe3 ...)
        expr
        env)
     #`(interpret-syntax-fragment/j
        prs
        (v #,(local-expand #`(prim-env-ref/j expr1 expr2) 'expression #f) expr3 ...)
        (pv noun pe3 ...)
        expr
        env)]))

(define-syntax (monadic-one/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ prs
        (v expr1 expr2 expr3) ; no ... ?
        (pv _ _ _)
        expr
        env)
     #`(interpret-syntax-fragment/j
        prs
        (v expr1 #,(local-expand #`(prim-env-ref/j expr2 expr3) 'expression #f))
        (pv verb noun)
        expr
        env)]))

(define-syntax (dyadic-two/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ prs
        (v expr1 expr2 expr3 r ...)
        (pv _ ...)
        expr
        env)
     #`(interpret-syntax-fragment/j
        prs
        (v #,(local-expand #`(prim-env-ref/j expr2 expr1 expr3) 'expression #f) r ...)
        (pv noun)
        expr
        env)]))

(define-syntax (adverb-three/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    #:datum-literals (/) ;other cases not implemented
    [(_ prs
        (v expr1 / expr3 r ...)
        (pv _ _ _ pr ...)
        expr
        env)
     #`(interpret-syntax-fragment/j
        prs
        (v (#,(local-expand #`(prim-env-ref/j expr1 /) 'expression #f) expr3) r ...)
        (pv noun pr ...) ; is verb the only possibility? (according to p&e, an adverb may apply to a noun)
        expr
        env)]))

(define-syntax (is-seven/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ prs
        (n:name/j =: x r ...)
        (name =: (~and (~var pos) (~or* noun verb)) p ...)
        expr
        env)
     #`(interpret-syntax-fragment/j
        prs
        (#,(local-expand #`(prim-env-ref/j n =: x) 'expression #f) r ...)
        (name p ...)
        expr
        #,(hash-set (syntax-e #`env) (syntax-e #`n) (syntax-e #`pos)))]))

;; SYNTAX INTERPRETER MAIN LOOP
(define-syntax (interpret-syntax-fragment/j stx)
  ; + Maintain a manual pos-stack: part-of-speech cannot be made a
  ;   type because types are unavailable before the end of expansion.
  ; + The value stack contains expanded program fragments as well as
  ;   unexpanded fragments, and therefore cannot use syntax class patterns
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    ; monadic application (pattern 0)
    [(_ prs vs (~and (~var ps) ((~or* newline-marker =:) verb noun _ ...)) expr env)
     #`(monadic-zero/j prs vs ps expr env)]
    ; monadic application (pattern 1)
    [(_ prs vs (~and (~var ps) ((~or* newline-marker =. =: adverb verb noun) verb verb noun)) expr env)
     #`(monadic-one/j prs vs ps expr env)]
    ; dyadic application (pattern 2)
    [(_ prs vs (~and (~var ps) ((~or* newline-marker =: verb noun) noun verb noun _ ...)) expr env)
     #`(dyadic-two/j prs vs ps expr env)]
    ; adverb (pattern 3)
    [(_ prs vs (~and (~var ps) ((~or* newline-marker =: verb noun) (~or* verb noun) adverb _ ...)) expr env)
     #`(adverb-three/j prs vs ps expr env)]
    ; assignment (pattern 7)
    [(_ prs vs (~and (~var ps) (name =: (~or* verb noun) _ ...)) expr env)
     #`(is-seven/j prs vs ps expr env)]
    ; program termination condition
    [(_ (p ...) (newline-marker v) (newline-marker (~or* noun verb name)) () env)
     #`(begin p ... v)]
    ; empty program
    [(_ () (newline-marker) (newline-marker) () env)
     #`(void)]
    ; program ending with a newline
    [(_ p (newline-marker) (newline-marker) (~rest r) env)
     #`(interpret-syntax-fragment/j p () () r env)]
    ; line termination condition for values
    ; place line result on value stack, reset pos stack, and continue
    [(_ (p ...) (newline-marker v) (newline-marker (~or* noun verb name)) expr env)
     #`(interpret-syntax-fragment/j (p ... v) () () expr env)]
    ; end-of-line encounter (newline-marker) with blank line skip (= multiple newline-markers)
    [(_ p (vs ...) (ps ...) (:newline-marker/j ...+ ~rest r) env)
     #`(interpret-syntax-fragment/j p (newline-marker vs ...) (newline-marker ps ...) r env)]
    ; noun encounter: immediate expansion to racket array
    [(_ p (vs ...) (ps ...) (e:noun/j ~rest r) env)
     #`(interpret-syntax-fragment/j p (e.expansion vs ...) (noun ps ...) r env)]
    ; verb encounter: placement on stack as-is (execution context currently unknown)
    [(_ p (vs ...) (ps ...) (e:verb/j ~rest r) env)
     #`(interpret-syntax-fragment/j p (e vs ...) (verb ps ...) r env)]
    ; adverb encounter
    [(_ p (vs ...) (ps ...) (e:adverb/j ~rest r) env)
     #`(interpret-syntax-fragment/j p (e vs ...) (adverb ps ...) r env)]
    ; name encounter: assignment case
    [(_ p (=: vs ...) (=: ps ...) (e:name/j ~rest r) env)
     #`(interpret-syntax-fragment/j p (e =: vs ...) (name =: ps ...) r env)]
    ; name encounter: reference case
    [(_ p (vs ...) (ps ...) (e:name/j ~rest r) env)
     #`(interpret-syntax-fragment/j p
                                    (e vs ...)
                                    (#,(hash-ref (syntax-e #`env) (syntax-e #`e)) ps ...)
                                    r
                                    env)]
    ; assignment symbol encounter
    ; =: corresponds to define & provide
    ; =. corresponds to scoped define or let form
    [(_ p (vs ...) (ps ...) (=: ~rest r) env)
     #`(interpret-syntax-fragment/j p (=: vs ...) (=: ps ...) r env)]
    ; debug
    [(_ prs ls pos expr env)
     #`(raise-syntax-error 'interpret-syntax-fragment/j
                           (string-append "debug condition" "\n\t"
                                          "program stack:\t" (~a 'prs) "\n\t"
                                          "line stack:\t" (~a 'ls) "\n\t"
                                          "p-o-s stack:\t" (~a 'pos) "\n\t"
                                          "current exp:\t" (~a 'expr) "\n\t"
                                          "syntax env:\t" (~a 'env)))]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (newline-marker)) #`(tr:#%module-begin)] ; empty program
    [(_ exprs) #`(tr:#%module-begin
               (interpret-syntax-fragment/j () () () exprs #,(hasheq)))])) ;env is an assoc list

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))

; This allows embedding into racket as a string (also useful for rackunit tests)
; However, breaking hygiene with syntax->datum is required (possible to restrict breakage to only 'define')?
; see https://stackoverflow.com/questions/49669142/what-is-difference-between-datum-syntax-and-syntax-in-define-syntax-body
; possibly required to manage '"': https://docs.racket-lang.org/udelim/index.html?q=udelim
(define-syntax (j stx)
      #`(interpret-syntax-fragment/j
         ()
         ()
         ()
         #,(datum->syntax stx (append (lex/j (open-input-string (cadr (syntax->datum stx)))) '(§)))
         ()))