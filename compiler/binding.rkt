#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/syntax-utils
  leo/compiler/type-sexp)

(data binding
  (identifier-option : (Option Identifier))
  (type : Type))

(define-type Scope (Stackof Binding))

(define scope : (-> Binding * Scope) stack)

(define null-scope : Scope null)

; TODO: Remove when not needed
(define (binding-syntax ($binding : Binding)) : Syntax
  (or (binding-identifier-option $binding) null-syntax))

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
