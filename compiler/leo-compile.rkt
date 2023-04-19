#lang leo/typed

(require 
  racket/port
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expressions
  leo/compiler/syntax-expressions
  leo/compiler/program
  leo/compiler/syntax-utils
  leo/compiler/ingredients
  leo/compiler/module-syntax
  leo/parser/sexp-parser)

(define (leo-compile ($sexp-list : (Listof Sexp))) : (Pairof Sexp Structure)
  (expressions-sexp-structure
    (syntax-list-expressions
      (map make-syntax
        (map sexp-datum $sexp-list)))))

(define (leo-compile-any ($any : Any)) : (Option Syntax)
  (expressions-syntax-option
    (syntax-list-expressions 
      (syntax-syntax-list (any-syntax $any)))))

(define (leo-compile-any-list ($any-list : (Listof Any))) : (Listof Syntax)
  (reverse
    (ingredients-top-level-syntax-stack
      (program-ingredients
        (syntax-list-program
          (map syntax-normalize (map any-syntax $any-list)))))))

(define (leo-compile-port ($port : Input-Port)) : (Listof Syntax)
  (define $string (port->string $port))
  (define $sexp-list
    (bind $result (parse-sexp-list $string)
      (cond
        ((failure? $result) (error (format "parse error: ~s ~s" $port $result)))
        (else $result))))
  (define $syntax-list
    (map
      (lambda (($sexp : Sexp)) (datum->syntax #f $sexp))
      $sexp-list))
  (leo-compile-any-list $syntax-list))
