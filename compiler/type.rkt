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

(define-type Packet (Stackof Value))

(define packet : (-> Value * Packet) stack)

(data racket)

(data field 
  (symbol : Symbol) 
  (structure : Structure))

; TODO: "Choice" is a wrong term. It should be something like "options", "alternatives".
; Then, the act of selection should be called "select" or "pick".
; Then, the act of switching should be called "switch", "handle".
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

(define-syntax (recipe! $syntax)
  (syntax-case $syntax (does)
    ((_ $from ... (does $to ...))
      (syntax 
        (arrow 
          (structure $from ...) 
          (structure $to ...))))))

; --------------------------------------------------------------------------

(define structure : (-> Type * Structure) stack)

(define null-structure null)

(define racket-type (racket))

(define (racket-field ($symbol : Symbol)) 
  (field! $symbol racket-type))

(define type-type (universe 0))
(define universe-type type-type)

(define (type-universe ($type : Type)) : Universe
  (if (universe? $type) 
    (universe (add1 (universe-index $type)))
    universe-type))
