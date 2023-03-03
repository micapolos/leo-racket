#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/option
  leo/typed/testing
  leo/typed/stack
  leo/compiler/racket
  leo/compiler/syntax-utils
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/type
  leo/compiler/typed
  leo/compiler/type-utils
  leo/compiler/expression-resolve
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (package-resolve-expression
  ($package : Package)
  ($expression : Expression))
  : (Option Expression)
  (package-resolve-field-expression $package $expression))

; ---------------------------------------------------------------------

(define (package-resolve-field-expression
  ($package : Package)
  ($expression : Expression))
  : (Option Expression)
  (option-bind (package-rhs-option $package) $rhs-package
    (option-stack-first 
      (map 
        (lambda (($lhs-expression : Expression)) 
          (expression-resolve-expression $lhs-expression $expression))
        (package-expression-stack $rhs-package)))))

(check-equal?
  (option-map
    (package-resolve-field-expression
      (package
        syntax-a
        (structure
          (field `point 
            (stack
              (field `b (stack (racket `b2))) 
              (field `c (stack (racket `c2))) 
              (field `d (stack (racket `d2)))))))
        (expression syntax-b (field `b null)))
    expression-typed-datum)
  (typed `(unsafe-vector-ref a 0) (field `b (stack (racket `b2)))))

(check-equal?
  (package-resolve-field-expression
    (package
      syntax-a
      (structure
        (field `point 
          (stack
            (field `b (stack (racket `b2))) 
            (field `c (stack (racket `c2))) 
            (field `d (stack (racket `d2)))))))
    (expression syntax-b (field `e null)))
  #f)
