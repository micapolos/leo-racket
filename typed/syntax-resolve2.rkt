#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/option
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/stack
  leo/typed/type-match
  leo/typed/syntax-get
  leo/typed/syntaxes
  leo/typed/syntax-match
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/type-parse
  leo/typed/type-decompile
  leo/typed/testing)

(define (syntax-stack-resolve ($syntax-stack : (Stackof Syntax))) : (Option Syntax)
  (or (syntax-stack-resolve-get $syntax-stack)))

(define (syntax-stack-resolve-get ($syntax-stack : (Stackof Syntax))) : (Option Syntax)
  (and
    (= (length $syntax-stack) 2)
    (let (($rhs-type (syntax-type (top $syntax-stack))))
      (and
        (tuple? $rhs-type)
        (let (($symbol (tuple-symbol $rhs-type))
              ($type-list (tuple-type-list $rhs-type)))
          (and 
            (equal? $symbol `get) 
            (= (length $type-list) 1)
            (syntax-get (pop-top $syntax-stack) (car $type-list))))))))

(check-equal?
  (option-map
    (syntax-stack-resolve-get
      (stack
        (syntax-with-type #`a (tuple `tuple (list number-type string-type)))
        (syntax-with-type null-syntax (tuple `get (list (tuple `number null))))))
    syntax-typed-datum)
  (typed `(unsafe-car a) number-type))
