#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/scope
  leo/compiler/package)

(data compiler 
  (scope : Scope) 
  (package : Package))

(define null-compiler (compiler null-scope null-package))
