#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  racket/unsafe/ops
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/environment
  leo/compiler/expressions
  leo/compiler/value
  leo/compiler/value-sexp
  leo/compiler/syntax-expressions
  leo/compiler/syntax-utils)

(define-namespace-anchor leo-namespace-anchor)

(define (leo-eval-sexp-list ($sexp-list : (Listof Sexp)))
  (define $expressions 
    (syntax-list-expressions 
      (map make-syntax (map sexp-datum $sexp-list))))
  (define $eval-sexp-list
    (any-structure-sexp-list
      (bind $list
        (call-with-values
          (lambda ()
            (eval 
              (syntax->datum (expressions-syntax $expressions))
              (namespace-anchor->namespace leo-namespace-anchor)))
          (ann list (-> Any * (Listof Any))))
        (or (single $list) $list))
      (expressions-structure $expressions)))
  (cond
    ((= (length $eval-sexp-list) 1) (car $eval-sexp-list))
    (else $eval-sexp-list)))

(define (leo-eval ($sexp : Sexp)) : Sexp
  (leo-eval-sexp-list
    (cond
      ((list? $sexp) $sexp)
      (else (list $sexp)))))

(check-equal?
  (leo-eval 
    `(
      1
      (add 2) 
      text
      (append " ")
      (append "pieces")
      (do
        text
        (append "!!!"))))
  "3 pieces!!!")
