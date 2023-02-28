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

(define (binding-resolve 
  ($binding : Binding)
  ($typed-syntax-stack : (Stackof (Typed Syntax Type)))
  ($srcloc : srcloc))
  : (Option (Typed Syntax Type))
  (define $type-stack 
    (map (ann typed-type (-> (Typed Syntax Type) Type)) $typed-syntax-stack))
  (define $dynamic-typed-syntax-stack 
    (filter typed-syntax-is-dynamic? $typed-syntax-stack))
  (define $dynamic-syntax-stack 
    (map (ann typed-value (-> (Typed Syntax Type) Syntax)) $dynamic-typed-syntax-stack))
  (and
    (type-stack-check? $type-stack (binding-param-type-stack $binding))
    (typed
      (make-syntax
        $srcloc
        (if (binding-function? $binding)
          `(,(binding-identifier $binding)
            ,@(reverse $dynamic-syntax-stack))
          (binding-identifier $binding)))
      (binding-return-type $binding))))

(check-equal?
  (bind $resolved
    (binding-resolve 
      (binding (stack type-a type-b) type-c identifier-d #f)
      (stack (typed syntax-a type-a) (typed syntax-b type-b))
      test-srcloc)
    (and $resolved (typed-syntax->typed-sourced $resolved)))
  (typed (sourced `d srcloc-d) type-c))

(check-equal?
  (bind $resolved
    (binding-resolve 
      (binding (stack type-a type-b) type-c identifier-d #t)
      (stack (typed syntax-a type-a) (typed syntax-b type-b))
      test-srcloc)
    (and $resolved (typed-syntax->typed-sourced $resolved)))
  (typed (sourced `(d a b) test-srcloc) type-c))

(check-equal?
  (binding-resolve 
    (binding (stack type-a type-b) type-c identifier-d #t)
    (stack (typed syntax-a type-a) (typed syntax-c type-c))
    test-srcloc)
  #f)

; ----------------------------------------------------------------------------

(define
  (binding-stack-resolve
    ($binding-stack : (Stackof Binding))
    ($typed-syntax-stack : (Stackof (Typed Syntax Type)))
    ($srcloc : srcloc))
  : (Option (Typed Syntax Type))
  (and
    (not (null? $binding-stack))
    (or
      (binding-resolve (car $binding-stack) $typed-syntax-stack $srcloc)
      (binding-stack-resolve (cdr $binding-stack) $typed-syntax-stack $srcloc))))

(check-equal?
  (bind $resolved
    (binding-stack-resolve 
      (stack 
        (binding (stack type-a) type-b identifier-b #t)
        (binding (stack type-c) type-d identifier-d #t))
      (stack typed-syntax-a)
      test-srcloc)
    (and $resolved (typed-syntax->typed-sourced $resolved)))
  (typed (sourced `(b a) test-srcloc) type-b))

(check-equal?
  (bind $resolved
    (binding-stack-resolve 
      (stack 
        (binding (stack type-a) type-b identifier-b #t)
        (binding (stack type-c) type-d identifier-d #t))
      (stack typed-syntax-c)
      test-srcloc)
    (and $resolved (typed-syntax->typed-sourced $resolved)))
  (typed (sourced `(d c) test-srcloc) type-d))

(check-equal?
  (binding-stack-resolve 
    (stack 
      (binding (stack type-a) type-b identifier-b #t)
      (binding (stack type-c) type-d identifier-d #t))
    (stack typed-syntax-d)
    test-srcloc)
  #f)
