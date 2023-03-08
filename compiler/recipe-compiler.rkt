#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/scope
  leo/compiler/recipe-package)

(data recipe-compiler 
  (scope : Scope) 
  (recipe-package : Recipe-Package))

(define null-recipe-compiler 
  (recipe-compiler null-scope null-recipe-package))
