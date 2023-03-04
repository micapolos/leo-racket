#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/testing
  leo/compiler/racket
  leo/compiler/binding
  leo/compiler/package
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (package-module-syntax ($package : Package)) : Syntax
  (define $syntax (package-syntax $package))
  (define $structure (package-structure $package))
  (define $scope (structure-generate-scope $structure))
  (define $tmp-stack (scope-identifier-stack $scope))
  (make-syntax 
    `(module leo racket/base
      ,(scope-module-syntax $scope)
      ,@(case (length $tmp-stack)
        ((0) null)
        ((1)
          (list 
            `(define 
              ,(car $tmp-stack) 
              ,$syntax)))
        (else 
          (list 
            `(define-values 
              (,@(reverse $tmp-stack)) 
              ,$syntax)))))))

(define (scope-module-syntax ($scope : Scope)) : Syntax
  (make-syntax 
    `(module* scope 
      (define scope ,(scope-syntax $scope)))))

(define (scope-syntax ($scope : Scope)) : Syntax
  (make-syntax 
    `(scope
      ,@(reverse (map binding-syntax $scope)))))

(define (binding-syntax ($binding : Binding)) : Syntax
  (make-syntax
    `(binding 
      ,(type-syntax (binding-type $binding))
      ,(option-bind (binding-identifier-option $binding) $identifier
        `(syntax ,(syntax-e $identifier))))))

(define (type-syntax ($type : Type)) : Syntax
  (make-syntax
    (cond
      ((racket? $type)
        `(racket 
          ,(cond
            ((symbol? (racket-any $type)) `(quote ,(racket-any $type)))
            (else (error "not sexp")))))
      ((field? $type)
        `(field 
          (quote ,(field-symbol $type))
          ,(structure-syntax (field-structure $type))))
      ((arrow? $type)
        `(arrow
          ,(structure-syntax (arrow-lhs-structure $type))
          ,(structure-syntax (arrow-rhs-structure $type))))
      ((a? $type)
        `(a
          ,(structure-syntax (a-structure $type)))))))

(define (structure-syntax ($structure : Structure)) : Syntax
  (make-syntax
    `(structure ,@(reverse (map type-syntax $structure)))))

(check-equal?
  (syntax->datum
    (package-module-syntax
      (package #`pkg
        (structure static-type-a))))
  `(module leo racket/base
    (module* scope 
      (define scope 
        (scope
          (binding (field 'a (structure)) #f))))))

(check-equal?
  (syntax->datum
    (package-module-syntax
      (package #`pkg
        (structure static-type-a dynamic-type-b))))
  `(module leo racket/base
    (module* scope 
      (define scope 
        (scope
          (binding (field 'a (structure)) #f)
          (binding (racket 'b) #'tmp-b))))
    (define tmp-b pkg)))

(check-equal?
  (syntax->datum
    (package-module-syntax
      (package #`pkg
        (structure dynamic-type-a static-type-b dynamic-type-c))))
  `(module leo racket/base
    (module* scope 
      (define scope 
        (scope
          (binding (racket 'a) #'tmp-a)
          (binding (field 'b (structure)) #f)
          (binding (racket 'c) #'tmp-c))))
    (define-values (tmp-a tmp-c) pkg)))
