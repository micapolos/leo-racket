#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/option
  leo/typed/base
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/sourced
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/typed-syntax
  leo/compiler/type-check
  leo/compiler/binding)

(define (binding-resolve-type
  ($binding : Binding)
  ($type : Type)
  ($srcloc : srcloc))
  : (Option Typed-Syntax)
  (and
    (type-check? $type (binding-type $binding))
    (typed
      (make-syntax $srcloc (syntax-e (binding-identifier $binding)))
      $type)))

(check-equal?
  (option-bind
    (binding-resolve-type
      (binding type-a identifier-a) type-a srcloc-a)
    $resolved
    (typed-syntax->typed-sourced $resolved))
  (typed (sourced `a srcloc-a) type-a))

(check-equal?
  (binding-resolve-type
    (binding type-a identifier-a) type-b srcloc-a)
  #f)

; -----------------------------------------------------------------------

(define (binding-resolve-get-typed-syntax
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
          (a-type $field-type)
          (syntax-srcloc (typed-value $typed-syntax)))))))

(check-equal?
  (option-bind
    (binding-resolve-get-typed-syntax
      (binding type-a identifier-b)
      (typed syntax-a (field `get (stack (a type-a)))))
    $resolved
    (typed-syntax->typed-sourced $resolved))
  (typed (sourced `b srcloc-a) type-a))

(check-equal?
  (binding-resolve-get-typed-syntax
    (binding type-a identifier-b)
    (typed syntax-a (field `not-get (stack (a type-a)))))
  #f)

(check-equal?
  (binding-resolve-get-typed-syntax
    (binding type-a identifier-b)
    (typed syntax-a (field `get (stack type-a))))
  #f)

(check-equal?
  (binding-resolve-get-typed-syntax
    (binding type-a identifier-b)
    (typed syntax-a (field `get (stack (a type-b)))))
  #f)

; -----------------------------------------------------------------------

(define (arrow-binding-resolve-sourced-typed-syntax-stack
  ($binding : Binding)
  ($sourced-typed-syntax-stack : (Sourced (Stackof Typed-Syntax))))
  : (Option Typed-Syntax)
  (define $binding-type (binding-type $binding))
  (define $srcloc (sourced-srcloc $sourced-typed-syntax-stack))
  (define $typed-syntax-stack (sourced-value $sourced-typed-syntax-stack))
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
              ,(binding-identifier $binding)
              ,@(reverse $dynamic-syntax-stack)))
          $arrow-rhs-type)))))

(check-equal?
  (option-bind
    (arrow-binding-resolve-sourced-typed-syntax-stack
      (binding (arrow (stack type-a type-b) type-c) identifier-d)
      (sourced (stack typed-syntax-a typed-syntax-b) srcloc-c))
    $resolved
    (typed-syntax->typed-sourced $resolved))
  (typed (sourced `(d a b) srcloc-c) type-c))

(check-equal?
  (arrow-binding-resolve-sourced-typed-syntax-stack
    (binding (arrow (stack type-a type-b) type-c) identifier-d)
    (sourced (stack typed-syntax-b typed-syntax-a) srcloc-c))
  #f)

; ------------------------------------------------------------------------

(define (binding-resolve-sourced-typed-syntax-stack
  ($binding : Binding)
  ($sourced-typed-syntax-stack : (Sourced (Stackof Typed-Syntax))))
  : (Option Typed-Syntax)
  (define $typed-syntax-stack (sourced-value $sourced-typed-syntax-stack))
  (define $single-typed-syntax (single $typed-syntax-stack))
  (or
    (and 
      $single-typed-syntax 
      (binding-resolve-get-typed-syntax $binding $single-typed-syntax))
    (arrow-binding-resolve-sourced-typed-syntax-stack 
      $binding 
      $sourced-typed-syntax-stack)))

; (check-equal?
;   (bind $resolved
;     (binding-resolve 
;       (binding (stack type-a type-b) type-c identifier-d #f)
;       (stack (typed syntax-a type-a) (typed syntax-b type-b))
;       test-srcloc)
;     (and $resolved (typed-syntax->typed-sourced $resolved)))
;   (typed (sourced `d srcloc-d) type-c))

; (check-equal?
;   (bind $resolved
;     (binding-resolve 
;       (binding (stack type-a type-b) type-c identifier-d #t)
;       (stack (typed syntax-a type-a) (typed syntax-b type-b))
;       test-srcloc)
;     (and $resolved (typed-syntax->typed-sourced $resolved)))
;   (typed (sourced `(d a b) test-srcloc) type-c))

; (check-equal?
;   (binding-resolve 
;     (binding (stack type-a type-b) type-c identifier-d #t)
;     (stack (typed syntax-a type-a) (typed syntax-c type-c))
;     test-srcloc)
;   #f)

; ; ----------------------------------------------------------------------------

; (define
;   (binding-stack-resolve
;     ($binding-stack : (Stackof Binding))
;     ($typed-syntax-stack : (Stackof (Typed Syntax Type)))
;     ($srcloc : srcloc))
;   : (Option (Typed Syntax Type))
;   (and
;     (not (null? $binding-stack))
;     (or
;       (binding-resolve (car $binding-stack) $typed-syntax-stack $srcloc)
;       (binding-stack-resolve (cdr $binding-stack) $typed-syntax-stack $srcloc))))

; (check-equal?
;   (bind $resolved
;     (binding-stack-resolve 
;       (stack 
;         (binding (stack type-a) type-b identifier-b #t)
;         (binding (stack type-c) type-d identifier-d #t))
;       (stack typed-syntax-a)
;       test-srcloc)
;     (and $resolved (typed-syntax->typed-sourced $resolved)))
;   (typed (sourced `(b a) test-srcloc) type-b))

; (check-equal?
;   (bind $resolved
;     (binding-stack-resolve 
;       (stack 
;         (binding (stack type-a) type-b identifier-b #t)
;         (binding (stack type-c) type-d identifier-d #t))
;       (stack typed-syntax-c)
;       test-srcloc)
;     (and $resolved (typed-syntax->typed-sourced $resolved)))
;   (typed (sourced `(d c) test-srcloc) type-d))

; (check-equal?
;   (binding-stack-resolve 
;     (stack 
;       (binding (stack type-a) type-b identifier-b #t)
;       (binding (stack type-c) type-d identifier-d #t))
;     (stack typed-syntax-d)
;     test-srcloc)
;   #f)
