#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/syntax-utils
  leo/compiler/type-sexp
  leo/compiler/type-utils)

(data binding
  (identifier-option : (Option Identifier))
  (type : Type))

(define-type Scope (Stackof Binding))

(define scope : (-> Binding * Scope) stack)

(define null-scope : Scope null)

(define (scope-structure ($scope : Scope)) : Structure
  (map binding-type $scope))

(define (scope-identifier-stack ($scope : Scope)) : (Stackof Identifier)
  (filter-false (map binding-identifier-option $scope)))

(define (binding-sexp ($binding : Binding)) : Sexp
  `(binding
    ,(option-app syntax->datum (binding-identifier-option $binding))
    ,(type-sexp (binding-type $binding))))

(define (scope-sexp ($scope : Scope)) : Sexp
  `(scope ,@(reverse (map binding-sexp $scope))))

(define (scope-push-identifier-stack-structure
  ($scope : Scope)
  ($identifier-stack : (Stackof Identifier))
  ($structure : Structure))
  : Scope
  (cond
    ((null? $structure) $scope)
    (else
      (define $top-type (top $structure))
      (define $pop-structure (pop $structure))
      (cond
        ((type-dynamic? $top-type)
          (scope-push-identifier-stack-structure
            (push $scope (binding (top $identifier-stack) $top-type))
            (pop $identifier-stack)
            $pop-structure))
        (else
          (scope-push-identifier-stack-structure
            (push $scope (binding #f $top-type))
            $identifier-stack
            $pop-structure))))))

(define (identifier-stack-structure-scope ($identifier-stack : (Stackof Identifier)) ($structure : Structure)) : Scope
  (reverse (scope-push-identifier-stack-structure null-scope $identifier-stack $structure)))

(check-equal?
  (scope-sexp
    (identifier-stack-structure-scope
      (stack identifier-a identifier-c)
      (structure dynamic-type-a static-type-b dynamic-type-c)))
  (scope-sexp
    (scope
      (binding identifier-a dynamic-type-a)
      (binding #f static-type-b)
      (binding identifier-c dynamic-type-c))))
