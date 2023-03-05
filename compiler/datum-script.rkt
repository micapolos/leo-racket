#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/stack
  leo/typed/base
  leo/compiler/script
  leo/compiler/syntax-script
  leo/compiler/syntax-utils)

(define (sexp-line ($sexp : Sexp)) : Line
  (syntax-line (make-syntax (sexp-datum $sexp))))

(define (sexp-list-script ($sexp-list : (Listof Sexp))) : Script
  (syntax-list-script 
    (map make-syntax (map sexp-datum $sexp-list))))

(define (sexp-script ($sexp : Sexp)) : Script
  (cond
    ((list? $sexp) (sexp-list-script $sexp))
    (else (stack (sexp-line $sexp)))))
