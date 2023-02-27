#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/binding
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/syntax-expand
  leo/typed/testing)

(data compiled 
  (binding-stack : (Stackof Binding))
  (compiled-syntax-stack : (Stackof Syntax))
  (value-syntax-stack : (Stackof Syntax)))

(define (compiled-push-value-syntax 
  ($compiled : Compiled) 
  ($syntax : Syntax)) : Compiled
  (struct-copy compiled $compiled
    (value-syntax-stack 
      (push 
        (compiled-value-syntax-stack $compiled) 
        $syntax))))

(define (compiled-push-compiled-syntax 
  ($compiled : Compiled) 
  ($syntax : Syntax)) : Compiled
  (struct-copy compiled $compiled
    (compiled-syntax-stack 
      (push 
        (compiled-compiled-syntax-stack $compiled) 
        $syntax))))

(define (compiled-parse-syntax
  ($compiled : Compiled)
  ($syntax : Syntax)) : Compiled
  (or
    (option-bind (syntax-expand $syntax) $syntax
      (compiled-push-value-syntax $compiled $syntax))
    (compiled-push-compiled-syntax $compiled $syntax)))

(define (compiled-parse-syntax-list
  ($compiled : Compiled)
  ($syntax-list : (Listof Syntax))) : Compiled
  (foldl
    (lambda (($syntax : Syntax) ($compiled : Compiled))
      (compiled-parse-syntax $compiled $syntax))
    $compiled
    $syntax-list))
