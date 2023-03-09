#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/expression
  leo/compiler/type)

(data select-package
  (selected-expression-option : (Option Expression))
  (choice-structure : Structure))

(define null-select-package 
  (select-package #f null-structure))
