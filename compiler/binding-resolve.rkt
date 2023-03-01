#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/option
  leo/typed/base
  leo/typed/testing
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
  : (Option Typed-Syntax)
  (define $binding-type (binding-type $binding))
  (and
    (type-check-symbol? $binding-type $symbol)
    (typed
      (binding-syntax $binding)
      $binding-type)))

(check-equal?
  (option-bind
    (binding-resolve-symbol
      (binding (field `a (stack type-b)) syntax-b) `a)
    $resolved
    (typed-syntax->typed-sourced $resolved))
  (typed (sourced `b srcloc-b) (field `a (stack type-b))))

(check-equal?
  (binding-resolve-symbol
    (binding (field `a (stack type-b)) syntax-b) `b)
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-type
  ($binding : Binding)
  ($type : Type))
  : (Option Typed-Syntax)
  (and
    (type-check? $type (binding-type $binding))
    (typed
      (binding-syntax $binding)
      $type)))

(check-equal?
  (option-bind
    (binding-resolve-type (binding type-a syntax-a) type-a)
    $resolved
    (typed-syntax->typed-sourced $resolved))
  (typed (sourced `a srcloc-a) type-a))

(check-equal?
  (binding-resolve-type (binding type-a syntax-a) type-b)
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-get-a-typed-syntax
  ($binding : Binding)
  ($typed-syntax : Typed-Syntax))
  : (Option Typed-Syntax)
  (define $type (typed-type $typed-syntax))
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
    (binding-resolve-get-a-typed-syntax
      (binding type-a syntax-b)
      (typed syntax-a (field `get (stack (a type-a)))))
    $resolved
    (typed-syntax->typed-sourced $resolved))
  (typed (sourced `b srcloc-b) type-a))

(check-equal?
  (binding-resolve-get-a-typed-syntax
    (binding type-a syntax-b)
    (typed syntax-a (field `not-get (stack (a type-a)))))
  #f)

(check-equal?
  (binding-resolve-get-a-typed-syntax
    (binding type-a syntax-b)
    (typed syntax-a (field `get (stack type-a))))
  #f)

(check-equal?
  (binding-resolve-get-a-typed-syntax
    (binding type-a syntax-b)
    (typed syntax-a (field `get (stack (a type-b)))))
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-get-symbol-typed-syntax
  ($binding : Binding)
  ($typed-syntax : Typed-Syntax))
  : (Option Typed-Syntax)
  (define $type (typed-type $typed-syntax))
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
    (binding-resolve-get-symbol-typed-syntax
      (binding (field `a (stack type-b)) syntax-b) 
      (typed syntax-a (field `get (stack (field `a null)))))
    $resolved
    (typed-syntax->typed-sourced $resolved))
  (typed (sourced `b srcloc-b) (field `a (stack type-b))))

(check-equal?
  (binding-resolve-get-symbol-typed-syntax
    (binding (field `a (stack type-b)) syntax-b) 
    (typed syntax-a type-a))
  #f)

(check-equal?
  (binding-resolve-get-symbol-typed-syntax
    (binding (field `a (stack type-b)) syntax-b) 
    (typed syntax-a (field `not-get (stack (field `a null)))))
  #f)

(check-equal?
  (binding-resolve-get-symbol-typed-syntax
    (binding (field `a (stack type-b)) syntax-b) 
    (typed syntax-a (field `get (stack (field `b null)))))
  #f)

(check-equal?
  (binding-resolve-get-symbol-typed-syntax
    (binding (field `a (stack type-b)) syntax-b) 
    (typed syntax-a (field `get (stack (field `a (stack type-a))))))
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-typed-syntax
  ($binding : Binding)
  ($typed-syntax : Typed-Syntax))
  : (Option Typed-Syntax)
  (or
    (binding-resolve-get-a-typed-syntax $binding $typed-syntax)
    (binding-resolve-get-symbol-typed-syntax $binding $typed-syntax)))

; -----------------------------------------------------------------------

(define (arrow-binding-resolve-typed-syntax-stack
  ($binding : Binding)
  ($typed-syntax-stack : (Stackof Typed-Syntax)))
  : (Option Typed-Syntax)
  (define $binding-type (binding-type $binding))
  (define $srcloc empty-srcloc)
  (define $type-stack (typed-stack->type-stack $typed-syntax-stack))
  (define $dynamic-syntax-stack (typed-syntax-stack->dynamic-syntax-stack $typed-syntax-stack))
  (and 
    (arrow? $binding-type)
    (let ()
      (define $arrow $binding-type)
      (define $arrow-lhs-type-stack (arrow-lhs-type-stack $arrow))
      (define $arrow-rhs-type (arrow-rhs-type $arrow))
      (and 
        (type-stack-check? $type-stack $arrow-lhs-type-stack)
        (typed
          (make-syntax 
            $srcloc 
            `(
              ,(binding-syntax $binding)
              ,@(reverse $dynamic-syntax-stack)))
          $arrow-rhs-type)))))

(check-equal?
  (option-bind
    (arrow-binding-resolve-typed-syntax-stack
      (binding (arrow (stack type-a type-b) type-c) syntax-d)
      (stack typed-syntax-a typed-syntax-b))
    $resolved
    (typed-syntax->typed-sourced $resolved))
  (typed (sourced `(d a b) empty-srcloc) type-c))

(check-equal?
  (arrow-binding-resolve-typed-syntax-stack
    (binding (arrow (stack type-a type-b) type-c) syntax-d)
    (stack typed-syntax-b typed-syntax-a))
  #f)

; ------------------------------------------------------------------------

(define (binding-resolve-typed-syntax-stack
  ($binding : Binding)
  ($typed-syntax-stack : (Stackof Typed-Syntax)))
  : (Option Typed-Syntax)
  (define $single-typed-syntax (single $typed-syntax-stack))
  (or
    (and 
      $single-typed-syntax
      (binding-resolve-typed-syntax $binding $single-typed-syntax))
    (arrow-binding-resolve-typed-syntax-stack $binding $typed-syntax-stack)))

; -----------------------------------------------------------------------

(define (binding-stack-resolve-typed-syntax-stack
  ($binding-stack : (Stackof Binding))
  ($typed-syntax-stack : (Stackof Typed-Syntax)))
  : (Option Typed-Syntax)
  (and 
    (not (null? $binding-stack))
    (or
      (binding-resolve-typed-syntax-stack 
        (top $binding-stack) 
        $typed-syntax-stack)
      (binding-stack-resolve-typed-syntax-stack 
        (pop $binding-stack) 
        $typed-syntax-stack))))
