#lang leo/typed

(require
  leo/compiler/any-sexp)

(data constant (any : Any))

(define (constant-sexp ($constant : Constant)) : Sexp
  `(constant ,(any-sexp (constant-any $constant))))
