#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/typed/syntax-match
  leo/typed/type-parse
  leo/typed/syntax-type)

(define
  (syntax-parse-racket ($syntax : Syntax))
  : (Option Syntax)
  (syntax-symbol-match-arg $syntax `racket $racket-rhs
    $racket-rhs))
