#lang leo/typed

(require
  leo/compiler/binding)

(define-type Scope (Stackof Binding))
