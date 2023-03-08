#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/compiler/type
  leo/compiler/package)

(data recipe-package
  (lhs-structure : Structure)
  (rhs-structure-option : (Option Structure))
  (body-package : (Option Package)))

(define null-recipe-package 
  (recipe-package null #f #f))
