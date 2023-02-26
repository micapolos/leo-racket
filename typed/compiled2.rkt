#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/binding2
  leo/typed/option
  leo/typed/stack
  leo/typed/syntax-expand
  leo/typed/testing)

(data compiled2 
  (binding-stack : (Stackof Binding2))
  (compiled-syntax-stack : (Stackof Syntax))
  (value-syntax-stack : (Stackof Syntax)))

(define (compiled2-push-value-syntax 
  ($compiled : Compiled2) 
  ($syntax : Syntax)) : Compiled2
  (struct-copy compiled2 $compiled
    (value-syntax-stack 
      (push 
        (compiled2-value-syntax-stack $compiled) 
        $syntax))))

(define (compiled2-push-compiled-syntax 
  ($compiled : Compiled2) 
  ($syntax : Syntax)) : Compiled2
  (struct-copy compiled2 $compiled
    (compiled-syntax-stack 
      (push 
        (compiled2-compiled-syntax-stack $compiled) 
        $syntax))))

(define (compiled2-parse-syntax 
  ($compiled : Compiled2)
  ($syntax : Syntax)) : Compiled2
  (or
    (option-bind (syntax-expand $syntax) $syntax
      (compiled2-push-value-syntax $compiled $syntax))
    (compiled2-push-compiled-syntax $compiled $syntax)))
