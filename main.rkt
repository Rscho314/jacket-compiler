#lang racket/base

(module+ test
  (require rackunit
           "expander.rkt"
           math/array)
  )

(module+ main)

(module+ reader
  (require "reader.rkt")
  (provide read-syntax))