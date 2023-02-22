#lang racket/base

(provide (all-defined-out))

(require 
  leo/typed/decompiler
  leo/typed/any-leo-string
  leo/typed/type-parse)

(define (leo-display $any $type)
  (display (any-leo-string (any-type-decompile $any (any-parse-type $type)))))
