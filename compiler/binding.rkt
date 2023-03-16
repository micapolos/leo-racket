#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/generate-temporary)

(data binding
  (type : Type)
  (identifier-option : (Option Identifier)))

(define (empty-binding ($type : Type)) : Binding
  (binding $type #f))

(define (binding-sexp ($binding : Binding)) : Sexp
  `(binding
    ,(type-sexp (binding-type $binding))
    ,(option-app syntax-e (binding-identifier-option $binding))))
