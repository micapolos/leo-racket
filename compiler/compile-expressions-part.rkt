#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/expressions
  leo/compiler/expressions-part
  leo/compiler/type
  leo/compiler/scope)

(define compile-expressions-part-parameter : (Parameterof (-> Scope (Listof Syntax) Expressions-Part))
  (make-parameter
    (lambda (($scope : Scope) ($syntax-list : (Listof Syntax))) : Expressions-Part
      (expressions-part
        (expressions #`recurse (structure (racket)))))))

(define (compile-expressions-part
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) : Expressions-Part
  ((compile-expressions-part-parameter) $scope $syntax-list))

(define (recursive-compile-expressions-part
  ($recurse : (-> Scope (Listof Syntax) Expressions-Part))
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) : Expressions-Part
  (parameterize ((compile-expressions-part-parameter $recurse))
    (compile-expressions-part $scope $syntax-list)))
