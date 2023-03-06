#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  racket/unsafe/ops
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/environment
  leo/compiler/package
  leo/compiler/value
  leo/compiler/value-sexp
  leo/compiler/syntax-package
  leo/compiler/syntax-utils)

(define-namespace-anchor leo-namespace-anchor)

(define (leo-eval-sexp-list ($sexp-list : (Listof Sexp)))
  (define $package 
    (syntax-list-package 
      (map make-syntax (map sexp-datum $sexp-list))))
  (define $eval-sexp-list
    (map value-sexp
      (map value
        (call-with-values
          (lambda ()
            (eval 
              (syntax->datum (package-syntax $package))
              (namespace-anchor->namespace leo-namespace-anchor)))
          (ann list (-> Any * (Listof Any))))
        (reverse (package-structure $package)))))
  (cond
    ((= (length $eval-sexp-list) 1) (car $eval-sexp-list))
    (else $eval-sexp-list)))

(define (leo-eval ($sexp : Sexp))
  (leo-eval-sexp-list
    (cond
      ((list? $sexp) $sexp)
      (else (list $sexp)))))

(check-equal?
  (leo-eval 
    `(
      (int 1) 
      (plus (int 2)) 
      text 
      (plus " ")
      (plus "pieces")
      (do 
        text
        (plus "!!!"))))
  "3 pieces!!!")

(leo-eval
  `(
    foo bar))