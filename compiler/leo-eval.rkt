#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/unsafe/ops
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/environment
  leo/compiler/package
  leo/compiler/value
  leo/compiler/value-datum
  leo/compiler/syntax-package
  leo/compiler/syntax-utils)

(define-namespace-anchor leo-namespace-anchor)

(define (leo-eval-datum-list ($datum-list : (Listof Datum)))
  (define $package (syntax-list-package (map make-syntax $datum-list)))
  (define $eval-datum-list
    (map value-datum
      (map value
        (call-with-values
          (lambda ()
            (eval 
              (syntax->datum (package-syntax $package))
              (namespace-anchor->namespace leo-namespace-anchor)))
          (ann list (-> Any * (Listof Any))))
        (reverse (package-structure $package)))))
  (cond
    ((= (length $eval-datum-list) 1) (car $eval-datum-list))
    (else $eval-datum-list)))

(define (leo-eval ($datum : Datum))
  (leo-eval-datum-list
    (cond
      ((list? $datum) $datum)
      (else (list $datum)))))

(check-equal?
  (leo-eval `((int 1) (plus (int 2)) text (plus " pieces")))
  "3 pieces")
