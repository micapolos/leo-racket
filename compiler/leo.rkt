#lang racket/base

(provide (all-defined-out))

(require
  (for-syntax 
    racket/base
    syntax/strip-context
    leo/compiler/base-scope
    leo/compiler/leo-eval
    leo/compiler/leo-compile
    leo/compiler/package
    leo/compiler/compiler-plus-syntax
    leo/typed/syntax-match))

(define-syntax (leo $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      (let ()
        (define $sexp-list (map syntax->datum (syntax-e #`(body ...))))
        (define $sexp (leo-eval $sexp-list))
        #`(quote #,(datum->syntax $syntax $sexp))))))

(define-syntax (leoc $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      (replace-context $syntax
        (leo-compile-any #`(body ...))))))

(leoc (point (x 10) (y 20)))
