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
(define base-compiled (compiled base-bindings null))

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
  (compiled-plus-compiled-syntax 
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (compiled-with-syntaxes 
    $compiled
    (cons $syntax (compiled-syntaxes $compiled))))

(define
  (compiled-plus-syntax
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (let* (($bindings (compiled-bindings $compiled))
         ($plus-bindings (bindings-plus-syntax $bindings $syntax)))
    (cond
      ((bindings? $plus-bindings) 
        (struct-copy compiled $compiled (bindings $plus-bindings)))
      (else 
        (compiled-plus-compiled-syntax $compiled 
          (bindings-syntax $bindings $syntax))))))

(define 
  (compiled-plus-syntaxes
    ($compiled : Compiled)
    ($syntaxes : (Listof Syntax))) : Compiled
  (foldl 
    (lambda (($syntax : Syntax) ($compiled : Compiled))
      (compiled-plus-syntax $compiled $syntax))
    $compiled
    $syntaxes))
