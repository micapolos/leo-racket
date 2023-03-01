#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/option
  leo/typed/base
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/sourced
  leo/compiler/srcloc
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/typed-syntax
  leo/compiler/type-check
  leo/compiler/binding)

(define (binding-resolve-symbol
  ($binding : Binding)
  ($symbol : Symbol))
  : (Option Expression)
  (define $binding-type (binding-type $binding))
  (and
    (type-check-symbol? $binding-type $symbol)
    (expression (binding-syntax $binding) $binding-type)))

(check-equal?
  (option-bind
    (binding-resolve-symbol
      (binding (field `a (stack type-b)) syntax-b) `a)
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `b srcloc-b) (field `a (stack type-b))))

(check-equal?
  (binding-resolve-symbol
    (binding (field `a (stack type-b)) syntax-b) `b)
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-type
  ($binding : Binding)
  ($type : Type))
  : (Option Expression)
  (and
    (type-check? $type (binding-type $binding))
    (expression (binding-syntax $binding) $type)))

(check-equal?
  (option-bind
    (binding-resolve-type (binding type-a syntax-a) type-a)
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `a srcloc-a) type-a))

(check-equal?
  (binding-resolve-type (binding type-a syntax-a) type-b)
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-get-a-expression
  ($binding : Binding)
  ($expression : Expression))
  : (Option Expression)
  (define $type (expression-type $expression))
  (and 
    (field? $type) 
    (equal? (field-symbol $type) `get)
    (= (length (field-type-stack $type)) 1)
    (let ()
      (define $field-type (top (field-type-stack $type)))
      (and
        (a? $field-type)
        (binding-resolve-type 
          $binding 
          (a-type $field-type))))))

(check-equal?
  (option-bind
    (binding-resolve-get-a-expression
      (binding type-a syntax-b)
      (expression syntax-a (field `get (stack (a type-a)))))
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `b srcloc-b) type-a))

(check-equal?
  (binding-resolve-get-a-expression
    (binding type-a syntax-b)
    (expression syntax-a (field `not-get (stack (a type-a)))))
  #f)

(check-equal?
  (binding-resolve-get-a-expression
    (binding type-a syntax-b)
    (expression syntax-a (field `get (stack type-a))))
  #f)

(check-equal?
  (binding-resolve-get-a-expression
    (binding type-a syntax-b)
    (expression syntax-a (field `get (stack (a type-b)))))
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-get-symbol-expression
  ($binding : Binding)
  ($expression : Expression))
  : (Option Expression)
  (define $type (expression-type $expression))
  (and 
    (field? $type) 
    (equal? (field-symbol $type) `get)
    (= (length (field-type-stack $type)) 1)
    (let ()
      (define $field-type (top (field-type-stack $type)))
      (and
        (field? $field-type)
        (null? (field-type-stack $field-type))
        (binding-resolve-symbol
          $binding 
          (field-symbol $field-type))))))

(check-equal?
  (option-bind
    (binding-resolve-get-symbol-expression
      (binding (field `a (stack type-b)) syntax-b) 
      (expression syntax-a (field `get (stack (field `a null)))))
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `b srcloc-b) (field `a (stack type-b))))

(check-equal?
  (binding-resolve-get-symbol-expression
    (binding (field `a (stack type-b)) syntax-b) 
    (expression syntax-a type-a))
  #f)

(check-equal?
  (binding-resolve-get-symbol-expression
    (binding (field `a (stack type-b)) syntax-b) 
    (expression syntax-a (field `not-get (stack (field `a null)))))
  #f)

(check-equal?
  (binding-resolve-get-symbol-expression
    (binding (field `a (stack type-b)) syntax-b) 
    (expression syntax-a (field `get (stack (field `b null)))))
  #f)

(check-equal?
  (binding-resolve-get-symbol-expression
    (binding (field `a (stack type-b)) syntax-b) 
    (expression syntax-a (field `get (stack (field `a (stack type-a))))))
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-expression
  ($binding : Binding)
  ($expression : Expression))
  : (Option Expression)
  (or
    (binding-resolve-get-a-expression $binding $expression)
    (binding-resolve-get-symbol-expression $binding $expression)))

; -----------------------------------------------------------------------

(define (arrow-binding-resolve-expression-stack
  ($binding : Binding)
  ($expression-stack : (Stackof Expression)))
  : (Option Expression)
  (define $binding-type (binding-type $binding))
  (define $type-stack (expression-stack-type-stack $expression-stack))
  (define $dynamic-syntax-stack (expression-stack-dynamic-syntax-stack $expression-stack))
  (and 
    (arrow? $binding-type)
    (let ()
      (define $arrow $binding-type)
      (define $arrow-lhs-type-stack (arrow-lhs-type-stack $arrow))
      (define $arrow-rhs-type (arrow-rhs-type $arrow))
      (and 
        (type-stack-check? $type-stack $arrow-lhs-type-stack)
        (expression
          (make-syntax 
            `(
              ,(binding-syntax $binding)
              ,@(reverse $dynamic-syntax-stack)))
          $arrow-rhs-type)))))

(check-equal?
  (option-bind
    (arrow-binding-resolve-expression-stack
      (binding (arrow (stack type-a type-b) type-c) syntax-d)
      (stack expression-a expression-b))
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `(d a b) empty-srcloc) type-c))

(check-equal?
  (arrow-binding-resolve-expression-stack
    (binding (arrow (stack type-a type-b) type-c) syntax-d)
    (stack expression-b expression-a))
  #f)

; ------------------------------------------------------------------------

(define (binding-resolve-expression-stack
  ($binding : Binding)
  ($expression-stack : (Stackof Expression)))
  : (Option Expression)
  (define $single-expression (single $expression-stack))
  (or
    (and 
      $single-expression
      (binding-resolve-expression $binding $single-expression))
    (arrow-binding-resolve-expression-stack $binding $expression-stack)))

; -----------------------------------------------------------------------

(define (binding-stack-resolve-expression-stack
  ($binding-stack : (Stackof Binding))
  ($expression-stack : (Stackof Expression)))
  : (Option Expression)
  (and 
    (not (null? $binding-stack))
    (or
      (binding-resolve-expression-stack
        (top $binding-stack) 
        $expression-stack)
      (binding-stack-resolve-expression-stack 
        (pop $binding-stack) 
        $expression-stack))))
