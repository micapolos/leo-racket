#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/type
  leo/typed/bindings
  racket/function
  leo/typed/syntax-type)

(struct compiled 
  (
    (bindings : Bindings) 
    (syntaxes : (Listof Syntax)))
  #:transparent
  #:type-name Compiled)

(define null-compiled (compiled null-bindings null))

(define 
  (compiled-with-syntaxes
    ($compiled : Compiled)
    ($syntaxes : (Listof Syntax))) : Compiled
  (struct-copy compiled $compiled (syntaxes $syntaxes)))

(define 
  (compiled-with-bindings
    ($compiled : Compiled)
    ($bindings : Bindings)) : Compiled
  (struct-copy compiled $compiled (bindings $bindings)))

(define 
  (compiled-plus-syntax 
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (compiled-with-syntaxes 
    $compiled
    (cons $syntax (compiled-syntaxes $compiled))))

(define
  (compiled-parse-syntax
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (let* (($bindings (compiled-bindings $compiled))
         ($parsed-syntax (bindings-parse-syntax $bindings $syntax)))
    (cond
      ((syntax? $parsed-syntax)
        (compiled-plus-syntax $compiled $parsed-syntax))
      (else
        (define $parsed-bindings
          (bindings-parse-define-syntax $bindings $syntax))
        (cond
          ((bindings? $parsed-bindings) 
            (compiled-with-bindings $compiled $parsed-bindings))
          (else (error "dupa")))))))

(define 
  (compiled-parse-syntaxes
    ($compiled : Compiled)
    ($syntaxes : (Listof Syntax))) : Compiled
  (foldl 
    (lambda (($syntax : Syntax) ($compiled : Compiled))
      (compiled-parse-syntax $compiled $syntax))
    $compiled
    $syntaxes))
