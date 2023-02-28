#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/compiler/script
  leo/compiler/syntax-script)

(define (datum-line ($datum : Datum)) : Line
  (syntax-line (datum->syntax #f $datum)))

(define (datum-list-script ($datum-list : (Listof Datum))) : Script
  (syntax-list-script 
    (map 
      (lambda (($datum : Datum)) (datum->syntax #f $datum))
      $datum-list)))
