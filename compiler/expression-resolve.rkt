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
  leo/compiler/type-check)

(define (expression-resolve-symbol
  ($expression : Expression)
  ($symbol : Symbol))
  : (Option Expression)
  (define $expression-type (expression-type $expression))
  (and
    (type-check-symbol? $expression-type $symbol)
    (expression (expression-syntax $expression) $expression-type)))

(check-equal?
  (option-bind
    (expression-resolve-symbol
      (expression syntax-b (field `a (stack type-b))) `a)
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `b srcloc-b) (field `a (stack type-b))))

(check-equal?
  (expression-resolve-symbol
    (expression syntax-b (field `a (stack type-b))) `b)
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-type
  ($expression : Expression)
  ($type : Type))
  : (Option Expression)
  (and
    (type-check? $type (expression-type $expression))
    (expression (expression-syntax $expression) $type)))

(check-equal?
  (option-bind
    (expression-resolve-type (expression syntax-a type-a) type-a)
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `a srcloc-a) type-a))

(check-equal?
  (expression-resolve-type (expression syntax-a type-a) type-b)
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-get-a-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (define $type (expression-type $rhs-expression))
  (and 
    (field? $type) 
    (equal? (field-symbol $type) `get)
    (= (length (field-structure $type)) 1)
    (let ()
      (define $field-type (top (field-structure $type)))
      (and
        (a? $field-type)
        (let ()
          (define $structure (a-structure $field-type))
          (define $type-option (single $structure))
          (and $type-option
            (expression-resolve-type 
              $lhs-expression $type-option)))))))

(check-equal?
  (option-bind
    (expression-resolve-get-a-expression
      (expression syntax-b type-a)
      (expression syntax-a (field `get (structure (a (structure type-a))))))
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `b srcloc-b) type-a))

(check-equal?
  (expression-resolve-get-a-expression
    (expression syntax-b type-a)
    (expression syntax-a (field `not-get (structure (a (structure type-a))))))
  #f)

(check-equal?
  (expression-resolve-get-a-expression
    (expression syntax-b type-a)
    (expression syntax-a (field `get (stack type-a))))
  #f)

(check-equal?
  (expression-resolve-get-a-expression
    (expression syntax-b type-a)
    (expression syntax-a (field `get (structure (a (structure type-b))))))
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-get-symbol-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (define $type (expression-type $rhs-expression))
  (and 
    (field? $type) 
    (equal? (field-symbol $type) `get)
    (= (length (field-structure $type)) 1)
    (let ()
      (define $field-type (top (field-structure $type)))
      (and
        (field? $field-type)
        (null? (field-structure $field-type))
        (expression-resolve-symbol
          $lhs-expression
          (field-symbol $field-type))))))

(check-equal?
  (option-bind
    (expression-resolve-get-symbol-expression
      (expression syntax-b (field `a (stack type-b))) 
      (expression syntax-a (field `get (stack (field `a null)))))
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `b srcloc-b) (field `a (stack type-b))))

(check-equal?
  (expression-resolve-get-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a type-a))
  #f)

(check-equal?
  (expression-resolve-get-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a (field `not-get (stack (field `a null)))))
  #f)

(check-equal?
  (expression-resolve-get-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a (field `get (stack (field `b null)))))
  #f)

(check-equal?
  (expression-resolve-get-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a (field `get (stack (field `a (stack type-a))))))
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (or
    (expression-resolve-get-a-expression $lhs-expression $rhs-expression)
    (expression-resolve-get-symbol-expression $lhs-expression $rhs-expression)))

; -----------------------------------------------------------------------

(define (arrow-expression-resolve-expression-stack
  ($lhs-expression : Expression)
  ($rhs-expression-stack : (Stackof Expression)))
  : (Option Expression)
  (define $expression-type (expression-type $lhs-expression))
  (define $structure (expression-stack-structure $rhs-expression-stack))
  (define $dynamic-syntax-stack (expression-stack-dynamic-syntax-stack $rhs-expression-stack))
  (and 
    (arrow? $expression-type)
    (let ()
      (define $arrow $expression-type)
      (define $arrow-lhs-structure (arrow-lhs-structure $arrow))
      (define $arrow-rhs-structure (arrow-rhs-structure $arrow))
      (define $arrow-rhs-type (single $arrow-rhs-structure))
      (unless $arrow-rhs-type (error "TODO: arrow multi-rhs-type"))
      (and 
        (structure-check? $structure $arrow-lhs-structure)
        (expression
          (make-syntax 
            `(
              ,(expression-syntax $lhs-expression)
              ,@(reverse $dynamic-syntax-stack)))
          $arrow-rhs-type)))))

(check-equal?
  (option-bind
    (arrow-expression-resolve-expression-stack
      (expression syntax-d (arrow (stack type-a type-b) (stack type-c)))
      (stack expression-a expression-b))
    $resolved
    (expression-typed-sourced $resolved))
  (typed (sourced `(d a b) empty-srcloc) type-c))

(check-equal?
  (arrow-expression-resolve-expression-stack
    (expression syntax-d (arrow (stack type-a type-b) (stack type-c)))
    (stack expression-b expression-a))
  #f)

; ------------------------------------------------------------------------

(define (expression-resolve-expression-stack
  ($lhs-expression : Expression)
  ($rhs-expression-stack : (Stackof Expression)))
  : (Option Expression)
  (define $single-rhs-expression (single $rhs-expression-stack))
  (or
    (and
      $single-rhs-expression
      (expression-resolve-expression 
        $lhs-expression 
        $single-rhs-expression))
    (arrow-expression-resolve-expression-stack 
      $lhs-expression 
      $rhs-expression-stack)))

; -----------------------------------------------------------------------

(define (expression-stack-resolve-expression-stack
  ($lhs-expression-stack : (Stackof Expression))
  ($rhs-expression-stack : (Stackof Expression)))
  : (Option Expression)
  (and 
    (not (null? $lhs-expression-stack))
    (or
      (expression-resolve-expression-stack
        (top $lhs-expression-stack)
        $rhs-expression-stack)
      (expression-stack-resolve-expression-stack 
        (pop $lhs-expression-stack) 
        $rhs-expression-stack))))
