#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/expression
  leo/compiler/binder
  leo/compiler/syntax-utils
  leo/compiler/type-utils)

(define (syntax-type-content-tuple ($syntax : Syntax)) : Tuple
  (tuple
    (expression
      (syntax-do $syntax
        (lambda (($identifier : Identifier))
          (make-syntax
            `(cond
              ((racket? ,$identifier) `(cons 0 ,$identifier))
              ((field? ,$identifier) `(cons 1 ,$identifier))))))
      (recursive
        (choice!
          (field! `racket)
          (field! `field word-type (stackof-type (variable 0))))))))
