#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/binding2
  leo/typed/stack
  leo/typed/testing)

(data compiled2 
  ($binding-stack : (Stackof Binding2))
  ($compiled-syntax-stack : (Stackof Syntax))
  ($value-syntax-stack : (Stackof Syntax)))

(define (compiled2-parse-syntax 
  ($compiled : Compiled2)
  ($syntax : Syntax)) : Compiled2
  $compiled)
