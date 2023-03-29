#lang leo/typed

(require 
  leo/compiler/runtime-environment
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (expression-value ($expression : Expression)) : Value
  (define $syntax (expression-syntax $expression))
  (define $type (expression-type $expression))
  (define $sexp (syntax->datum $syntax))
  (define $value-any (environment-eval runtime-environment $sexp))
  (value $value-any $type))

(define (tuple-value-stack ($tuple : Tuple)) : (Stackof Value)
  (map expression-value $tuple))

(check-equal?
  (expression-value (text-expression "foo"))
  (value "foo" text-type))

(check-equal?
  (expression-value
    (expression 
      (make-syntax `(string-append "Hello, " "world!"))
      text-type))
  (value "Hello, world!" text-type))

; TODO: Why it fails, although values look equal? Are we dealing with prefab?
; (check-equal?
;   (expression-value (type-expression number-type))
;   (value number-type type-type))
