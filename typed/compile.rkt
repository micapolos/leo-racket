#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/testing
  leo/typed/syntax-match
  leo/typed/compiled)

(define (anys-compile ($anys : (Listof Any))) : (Listof Syntax)
  (syntaxes-compile (map any-syntax $anys)))

(define (syntaxes-compile ($syntax-list : (Listof Syntax))) : (Listof Syntax)
  (reverse 
    (compiled-syntax-list
      (compiled-parse-syntax-list
        null-compiled 
        $syntax-list))))

(define (sexps-compile ($sexps : (Listof Sexp))) : (Listof Sexp)
  (map
    syntax->datum
    (syntaxes-compile
      (map
        (lambda (($sexp : Sexp)) (cast-syntax (datum->syntax #f $sexp)))
        $sexps))))

(define (sexp-compile ($sexp : Sexp)) : Sexp
  (cond
    ((list? $sexp) (sexps-compile $sexp))
    (else (sexps-compile (list $sexp)))))
  
(check-equal?
  (sexp-compile `(1 "foo" (point 10 "bar")))
  `(1 "foo" (cons 10 "bar")))
