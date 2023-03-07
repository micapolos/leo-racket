#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/typed/stack
  leo/compiler/syntax-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/type
  leo/compiler/typed
  leo/compiler/type-utils
  leo/compiler/expression-resolve
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (expressions-resolve-expression
  ($expressions : Expressions)
  ($expression : Expression))
  : (Option Expressions)
  (expressions-rhs-resolve-expression $expressions $expression))

; ---------------------------------------------------------------------

(define (expressions-rhs-resolve-expression
  ($expressions : Expressions)
  ($expression : Expression))
  : (Option Expressions)
  (option-bind (expressions-rhs-option $expressions) $rhs-expressions
    (option-map
      (option-stack-first 
        (map
          (lambda (($lhs-expression : Expression)) 
            (expression-resolve-expression $lhs-expression $expression))
          (expressions-tuple $rhs-expressions)))
      expression-expressions)))

(check-equal?
  (option-map
    (expressions-rhs-resolve-expression
      (expressions
        syntax-a
        (structure
          (field `point 
            (stack
              (field `b (stack (racket))) 
              (field `c (stack (racket))) 
              (field `d (stack (racket)))))))
        (expression syntax-b (field `b null)))
    expressions-sexp-structure)
  (pair 
    `(unsafe-vector-ref a 0)
    (structure (field `b (stack (racket))))))

(check-equal?
  (expressions-rhs-resolve-expression
    (expressions
      syntax-a
      (structure
        (field `point 
          (stack
            (field `b (stack (racket))) 
            (field `c (stack (racket))) 
            (field `d (stack (racket)))))))
    (expression syntax-b (field `e null)))
  #f)
