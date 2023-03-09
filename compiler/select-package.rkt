#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type)

(data select-package
  (selection : (Option (Pairof Exact-Nonnegative-Integer Syntax)))
  (structure : Structure))

(define null-select-package 
  (select-package #f null-structure))

(define (select-package-plus-not 
  ($package : Select-Package)
  ($type : Type)) : Select-Package
  (select-package
    (select-package-selection $package)
    (push
      (select-package-structure $package)
      $type)))

(define (select-package-plus-the 
  ($package : Select-Package)
  ($expression : Expression)) : Select-Package
  (unless (not (select-package-selection $package))
    (error "already selected"))
  (select-package
    (pair 
      (length (select-package-structure $package))
      (expression-syntax $expression))
    (push
      (select-package-structure $package)
      (expression-type $expression))))

(define (select-package-expression
  ($package : Select-Package)) : Expression
  (define $selection (select-package-selection $package))
  (unless $selection (error "already selected"))
  (index-syntax-structure-select-expression 
    (car $selection)
    (cdr $selection)
    (select-package-structure $package)))
