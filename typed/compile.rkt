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

(define (datums-compile ($datums : (Listof Any))) : (Listof Any)
  (map
    syntax->datum
    (syntaxes-compile
      (map
        (lambda (($datum : Any)) (cast-syntax (datum->syntax #f $datum)))
        $datums))))

(define (datum-compile ($datum : Any)) : Any
  (cond
    ((list? $datum) (datums-compile $datum))
    (else (datums-compile (list $datum)))))

(check-equal?
  (datum-compile `(1 "foo" (point 10 "bar")))
  `(1 "foo" (immutable-vector 10 "bar")))
