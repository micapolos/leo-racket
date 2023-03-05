#lang typed/racket/base

(provide (all-defined-out))

(require
  (for-syntax 
    racket/base
    leo/compiler/compiler-plus-syntax))

(define-syntax (leo $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      #`(begin body ...))))
