#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/type-runtime
  leo/typed/option
  leo/typed/binding
  leo/typed/type-syntax
  leo/typed/syntax-match
  leo/typed/testing)

(define (binding-syntax ($binding : Binding)) : (Option Syntax)
  (option-map
    (cond
      ((constant-binding? $binding)
        #`(constant-binding
          (quote #,(constant-binding-symbol $binding))
          #,(type-syntax (constant-binding-type $binding))
          (quote #,(constant-binding-bound-symbol $binding))))
      ((function-binding? $binding)
        #`(function-binding
          (quote #,(function-binding-symbol $binding))
          (list #,@(map type-syntax (function-binding-param-types $binding)))
          #,(type-syntax (function-binding-return-type $binding))
          (quote #,(function-binding-bound-symbol $binding))))
      ((argument-binding? $binding) #f))
    cast-syntax))

(check-equal?
  (option-map 
    (binding-syntax (constant-binding `foo number `bar))
    syntax->datum)
  `(constant-binding 'foo number 'bar))

(check-equal?
  (option-map 
    (binding-syntax (function-binding `foo (list number string) boolean `bar))
    syntax->datum)
  `(function-binding 'foo (list number string) boolean 'bar))

(check-equal?
  (binding-syntax (argument-binding number #`foo))
  #f)
