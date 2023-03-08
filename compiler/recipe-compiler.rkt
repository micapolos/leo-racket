#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/scope
  leo/compiler/recipe-package)

(data recipe-compiler 
  (scope : Scope) 
  (package : Recipe-Package))

(define (null-recipe-compiler ($scope : Scope))
  (recipe-compiler $scope null-recipe-package))
