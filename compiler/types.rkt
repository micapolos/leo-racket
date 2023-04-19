#lang leo/typed

(require
  leo/compiler/type)

(data types
  (stack : (Stackof Type)))

(define null-types (infix null types))

(define (types-plus ($types : Types) ($type : Type)) : Types
  (types (push (types-stack $types) $type)))

(define-syntax (types! $syntax)
  (syntax-case $syntax ()
    ((_ $stx ...)
      #`(types (stack $stx ...)))))
