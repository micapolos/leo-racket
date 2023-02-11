#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/compiled)

(define (syntaxes-compile ($syntaxes : (Listof (Syntaxof Any)))) : (Listof (Syntaxof Any))
  (reverse 
    (compiled-syntaxes
      (compiled-parse-syntaxes null-compiled $syntaxes))))

(define (datums-compile ($datums : (Listof Datum)))
  (map
    syntax->datum
    (syntaxes-compile 
      (map
        (lambda (($datum : Datum)) (datum->syntax #f $datum))
        $datums))))

(datums-compile `("sdf" 1))
