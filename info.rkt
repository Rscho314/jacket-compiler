#lang info
(define collection "jacket-compiler")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "typed-racket-lib"))
(define scribblings '(("scribblings/jacket-compiler.scrbl" ())))
(define pkg-desc "A proof-of-concept J compiler using typed racket")
(define version "0.0")
(define pkg-authors '(raoul))
