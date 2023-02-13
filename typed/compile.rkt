#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/testing
  leo/typed/syntax-match
  leo/typed/compiled)

(define (syntaxes-compile ($syntaxes : (Listof Syntax))) : (Listof Syntax)
  (reverse 
    (compiled-syntaxes
      (compiled-plus-syntaxes null-compiled $syntaxes))))

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
  `(1 "foo" (immutable-vector 10 "bar")))
