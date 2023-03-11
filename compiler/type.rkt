#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  (for-syntax racket/base))

(define-type Type 
  (U 
    Racket
    Value
    Field 
    Choice 
    Arrow 
    Generic
    Specific
    Recursive 
    Variable
    Universe))

(define-type Structure (Stackof Type))

(data value (any : Any) (type : Type))

(data racket)

(data field 
  (symbol : Symbol) 
  (structure : Structure))

(data choice (type-stack : (Stackof Type)))

(data arrow
  (from-structure : Structure) 
  (to-structure : Structure))

(data generic (type : Type))

(data specific
  (type : Type) 
  (argument-type : Type))

(data recursive (type : Type))

(data variable (index : Exact-Nonnegative-Integer))

(data universe (index : Exact-Nonnegative-Integer))

; --------------------------------------------------------------------------

(define-syntax (field! $syntax)
  (syntax-case $syntax ()
    ((_ $symbol $type ...)
      (syntax (field $symbol (structure $type ...))))))

(define-syntax (choice! $syntax)
  (syntax-case $syntax ()
    ((_ $type ...)
      (syntax (choice (stack $type ...))))))

; --------------------------------------------------------------------------

(define structure stack)

(define null-structure null)

(define racket-type (racket))

(define (racket-field ($symbol : Symbol)) 
  (field! $symbol racket-type))

(define universe-type (universe 0))

(define (type-universe ($type : Type)) : Universe
  (if (universe? $type) 
    (universe (add1 (universe-index $type)))
    universe-type))
