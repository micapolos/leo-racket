#lang leo/typed

(define-type Body (Stackof Syntax))

(define body stack)

(define null-body : Body (body))
