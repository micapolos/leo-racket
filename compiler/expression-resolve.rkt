#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/option
  leo/typed/base
  leo/typed/testing
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/sexp-utils
  leo/compiler/syntax-utils
  leo/compiler/sourced
  leo/compiler/srcloc
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
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
  (option-app expression-sexp-type
    (expression-resolve-symbol
      (expression syntax-b (field `a (stack type-b))) `a))
  (pair `b (field `a (stack type-b))))

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
  (option-app expression-sexp-type
    (expression-resolve-type (expression syntax-a type-a) type-a))
  (pair `a type-a))

(check-equal?
  (expression-resolve-type (expression syntax-a type-a) type-b)
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-a-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (define $type (expression-type $rhs-expression))
  (and
    (a? $type) 
    (expression-resolve-type $lhs-expression (a-type $type))))

(check-equal?
  (option-app expression-sexp-type
    (expression-resolve-a-expression
      (expression syntax-b type-a)
      (expression syntax-a (a type-a))))
  (pair `b type-a))

(check-equal?
  (expression-resolve-a-expression
    (expression syntax-b type-a)
    (expression syntax-a (field `get (stack type-a))))
  #f)

(check-equal?
  (expression-resolve-a-expression
    (expression syntax-b type-a)
    (expression syntax-a (a type-b)))
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-symbol-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (define $type (expression-type $rhs-expression))
  (and
    (field? $type)
    (null? (field-structure $type))
    (expression-resolve-symbol
      $lhs-expression
      (field-symbol $type))))

(check-equal?
  (option-app expression-sexp-type
    (expression-resolve-symbol-expression
      (expression syntax-b (field `a (stack type-b))) 
      (expression syntax-a (field `a null))))
  (pair `b (field `a (stack type-b))))

(check-equal?
  (expression-resolve-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a type-a))
  #f)

(check-equal?
  (expression-resolve-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a (field `not-a null)))
  #f)

(check-equal?
  (expression-resolve-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a (field `a (stack type-a))))
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (or
    (expression-resolve-a-expression $lhs-expression $rhs-expression)
    (expression-resolve-symbol-expression $lhs-expression $rhs-expression)))

; -----------------------------------------------------------------------

(define (arrow-expression-resolve-tuple
  ($lhs-expression : Expression)
  ($rhs-tuple : Tuple))
  : (Option Package)
  (define $expression-type (expression-type $lhs-expression))
  (define $structure (tuple-structure $rhs-tuple))
  (define $dynamic-syntax-stack (tuple-dynamic-syntax-stack $rhs-tuple))
  (and 
    (arrow? $expression-type)
    (let ()
      (define $arrow $expression-type)
      (define $arrow-lhs-structure (arrow-lhs-structure $arrow))
      (define $arrow-rhs-structure (arrow-rhs-structure $arrow))
      (and 
        (structure-check? $structure $arrow-lhs-structure)
        (package
          (make-syntax 
            `(
              ,(expression-syntax $lhs-expression)
              ,@(reverse $dynamic-syntax-stack)))
          $arrow-rhs-structure)))))

(check-equal?
  (option-app package-sexp-structure
    (arrow-expression-resolve-tuple
      (expression syntax-d 
        (arrow 
          (stack type-a type-b) 
          (stack type-c type-d)))
      (stack expression-a expression-b)))
  (pair `(d a b) (structure type-c type-d)))

(check-equal?
  (arrow-expression-resolve-tuple
    (expression syntax-d (arrow (stack type-a type-b) (stack type-c)))
    (stack expression-b expression-a))
  #f)

; ------------------------------------------------------------------------

(define (expression-resolve-tuple
  ($lhs-expression : Expression)
  ($rhs-tuple : Tuple))
  : (Option Package)
  (define $single-rhs-expression (single $rhs-tuple))
  (or
    (and
      $single-rhs-expression
      (option-app expression-package
        (expression-resolve-expression 
          $lhs-expression 
          $single-rhs-expression)))
    (arrow-expression-resolve-tuple 
      $lhs-expression 
      $rhs-tuple)))

; -----------------------------------------------------------------------

(define (tuple-resolve-tuple
  ($lhs-tuple : Tuple)
  ($rhs-tuple : Tuple))
  : (Option Package)
  (and 
    (not (null? $lhs-tuple))
    (or
      (expression-resolve-tuple
        (top $lhs-tuple)
        $rhs-tuple)
      (tuple-resolve-tuple 
        (pop $lhs-tuple) 
        $rhs-tuple))))

; -----------------------------------------------------------------------

(define (tuple-expression-resolve
  ($lhs-tuple : Tuple)
  ($rhs-expression : Expression))
  : (Option Package)
  (or 
    (tuple-expression-resolve-arrow $lhs-tuple $rhs-expression)
    (option-app expression-package
      (option-app expression-resolve-expression
        (single $lhs-tuple)
        $rhs-expression))))

; -----------------------------------------------------------------------

(define (tuple-expression-resolve-arrow
  ($lhs-tuple : Tuple)
  ($rhs-expression : Expression))
  : (Option Package)
  (option-bind (tuple-lift-structure $lhs-tuple) $lhs-structure
    (option-bind (expression-lift-type $rhs-expression) $rhs-type
      (and 
        (field? $rhs-type) 
        (equal? (field-symbol $rhs-type) `giving)
        (expression-package
          (type-expression
            (arrow
              $lhs-structure 
              (field-structure $rhs-type))))))))

(check-equal?
  (option-app package-sexp-structure
    (tuple-expression-resolve-arrow
      (tuple (type-expression type-a) (type-expression type-b))
      (type-expression (field `giving (structure type-c type-d)))))
  (pair 
    null-sexp
    (structure
      (a 
        (arrow 
          (structure type-a type-b) 
          (structure type-c type-d))))))

; -----------------------------------------------------------------------

(define (tuple-resolve ($tuple : Tuple)) : (Option Package)
  (and
    (>= (length $tuple) 2))
    (tuple-expression-resolve (cdr $tuple) (car $tuple)))

; -----------------------------------------------------------------------

(define (tuple-do ($tuple : Tuple) ($fn : (-> Scope Package))) : Package
  (define $structure (tuple-structure $tuple))
  (define $dynamic-syntax-stack (tuple-dynamic-syntax-stack $tuple))
  (define $values-syntax (tuple-values-syntax-option $tuple))
  (define $scope (structure-generate-scope $structure))
  (define $fn-package ($fn $scope))
  (define $fn-syntax (package-syntax $fn-package))
  (define $fn-structure (package-structure $fn-package))
  (define $tmp-stack (scope-symbol-stack $scope))
  (package
    (make-syntax 
      (case (length $tmp-stack)
        ((0) $fn-syntax)
        ((1)
          `(let
            ((,(car $tmp-stack) ,$values-syntax))
            ,$fn-syntax))
        (else 
          `(let-values 
            ((,@(reverse $tmp-stack)) ,$values-syntax)
            ,$fn-syntax))))
    $fn-structure))

(check-equal?
  (package-sexp-structure
    (tuple-do
      (tuple static-expression-a)
      (lambda (($scope : Scope)) 
        (package 
          (make-syntax `(values ,@(scope-symbol-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    `(values)
    (structure static-type-a)))

(check-equal?
  (package-sexp-structure
    (tuple-do
      (tuple dynamic-expression-a static-expression-b)
      (lambda (($scope : Scope)) 
        (package 
          (make-syntax `(values ,@(scope-symbol-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    `(let ((tmp-a a)) (values tmp-a)) 
    (structure static-type-b dynamic-type-a)))

(check-equal?
  (package-sexp-structure
    (tuple-do
      (tuple dynamic-expression-a static-expression-b dynamic-expression-c)
      (lambda (($scope : Scope)) 
        (package 
          (make-syntax `(values ,@(scope-symbol-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    `(let-values ((tmp-a tmp-c) (values a c)) (values tmp-c tmp-a)) 
    (structure dynamic-type-c static-type-b dynamic-type-a)))

; -----------------------------------------------------------------------

(define (tuple-doing ($tuple : Tuple) ($fn : (-> Scope Package))) : (Option Package)
  (option-bind (structure-lift (tuple-structure $tuple)) $structure
    (define $dynamic-syntax-stack (tuple-dynamic-syntax-stack $tuple))
    (define $values-syntax (tuple-values-syntax-option $tuple))
    (define $scope (structure-generate-scope $structure))
    (define $fn-package ($fn $scope))
    (define $fn-syntax (package-syntax $fn-package))
    (define $fn-structure (package-structure $fn-package))
    (define $tmp-stack (scope-symbol-stack $scope))
    (define $arrow (arrow $structure $fn-structure))
    (package
      (cond
        ((type-dynamic? $arrow)
          (make-syntax `(lambda (,@(reverse $tmp-stack)) ,$fn-syntax)))
        (else null-syntax))
      (structure $arrow))))

(check-equal?
  (tuple-doing
    (tuple expression-a)
    (lambda (($scope : Scope)) null-package))
  #f)

(check-equal?
  (option-app package-sexp-structure
    (tuple-doing
      (tuple (type-expression static-type-a))
      (lambda (($scope : Scope)) 
        (package 
          (make-syntax `(values ,@(scope-symbol-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    null-sexp
    (structure 
      (arrow 
        (structure static-type-a) 
        (structure static-type-a)))))

(check-equal?
  (option-app package-sexp-structure
    (tuple-doing
      (tuple (type-expression static-type-a))
      (lambda (($scope : Scope)) 
        (package 
          (make-syntax `(values ,@(push (scope-symbol-stack $scope) `b)))
          (push (scope-structure $scope) dynamic-type-b)))))
  (pair 
    `(lambda () (values b))
    (structure 
      (arrow 
        (structure static-type-a) 
        (structure static-type-a dynamic-type-b)))))

(check-equal?
  (option-app package-sexp-structure
    (tuple-doing
      (tuple 
        (type-expression dynamic-type-a) 
        (type-expression static-type-b))
      (lambda (($scope : Scope)) 
        (package 
          (make-syntax `(values ,@(scope-symbol-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    `(lambda (tmp-a) (values tmp-a)) 
    (structure 
      (arrow 
        (structure dynamic-type-a static-type-b)
        (structure static-type-b dynamic-type-a)))))

(check-equal?
  (option-app package-sexp-structure
    (tuple-doing
      (tuple 
        (type-expression dynamic-type-a)
        (type-expression static-type-b)
        (type-expression dynamic-type-c))
      (lambda (($scope : Scope)) 
        (package 
          (make-syntax `(values ,@(scope-symbol-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    `(lambda (tmp-a tmp-c) (values tmp-c tmp-a))
    (structure 
      (arrow
        (structure dynamic-type-a static-type-b dynamic-type-c)
        (structure dynamic-type-c static-type-b dynamic-type-a)))))
