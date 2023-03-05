#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/type
  leo/compiler/generate-temporary)

(data binding
  (type : Type)
  (symbol-option : (Option Symbol)))

(define (empty-binding ($type : Type)) : Binding
  (binding $type #f))
