#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/option
  leo/typed/testing
  leo/typed/stack
  leo/compiler/racket
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/typed
  leo/compiler/type-utils
  leo/compiler/binding
  leo/compiler/binding-resolve
  leo/compiler/expression
  leo/compiler/expression-utils)

; TODO: unify Binding and Expression
(define (expression-binding ($expression : Expression)) : Binding
  (binding 
    (expression-type $expression)
    (expression-syntax $expression)))

(define (expression-stack-resolve 
  ($expression-stack : (Stackof Expression)))
  : (Option Expression)
  (expression-stack-resolve-field $expression-stack))

; ---------------------------------------------------------------------

(define (expression-stack-resolve-field
  ($expression-stack : (Stackof Expression)))
  : (Option Expression)
  (and
    (= (length $expression-stack) 2)
    (let ()
      (define $lhs-expression (pop-top $expression-stack))
      (define $rhs-expression (top $expression-stack))
      (define $lhs-type (expression-type $lhs-expression))
      (and
        (field? $lhs-type)
        (let ()
          (define $lhs-syntax (expression-syntax $lhs-expression))
          (define $lhs-type-stack (field-type-stack $lhs-type))
          (define $lhs-expression-stack 
            (syntax-type-stack-expression-stack $lhs-syntax $lhs-type-stack))
          (define $lhs-binding-stack 
            (map expression-binding $lhs-expression-stack))
          (define $resolved-expression-stack 
            (map 
              (lambda (($binding : Binding)) 
                (binding-resolve-expression $binding $rhs-expression))
              $lhs-binding-stack))
          (option-stack-first $resolved-expression-stack))))))

(check-equal?
  (option-map
    (expression-stack-resolve-field
      (stack
        (expression syntax-a 
          (field `point 
            (stack 
              (field `b (stack (racket `b2))) 
              (field `c (stack (racket `c2))) 
              (field `d (stack (racket `d2))))))
        (expression syntax-b
          (field `get (stack (field `b null))))))
    expression-typed-datum)
  (typed `(unsafe-vector-ref a 0) (field `b (stack (racket `b2)))))
