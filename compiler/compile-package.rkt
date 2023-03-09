#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/expressions
  leo/compiler/package
  leo/compiler/type
  leo/compiler/scope)

(define compile-package-parameter : (Parameterof (-> Scope (Listof Syntax) Package))
  (make-parameter
    (lambda (($scope : Scope) ($syntax-list : (Listof Syntax))) : Package
      (package
        (expressions #`recurse (structure (racket)))))))

(define (compile-package
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) : Package
  ((compile-package-parameter) $scope $syntax-list))

(define (recursive-compile-package
  ($recurse : (-> Scope (Listof Syntax) Package))
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) : Package
  (parameterize ((compile-package-parameter $recurse))
    (compile-package $scope $syntax-list)))
