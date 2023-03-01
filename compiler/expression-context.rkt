#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/expression
  leo/typed/stack)

(struct expression-context (
  (private-expression-stack : (Stackof Expression))
  (public-expression-stack : (Stackof Expression)))
  #:transparent
  #:type-name Expression-Context)

(define (expression-context-push-private 
  ($expression-context : Expression-Context)
  ($expression : Expression))
  : Expression-Context
  (struct-copy expression-context $expression-context
    (private-expression-stack 
      (push
        (expression-context-private-expression-stack $expression-context)
        $expression))))

(define (expression-context-push-public 
  ($expression-context : Expression-Context)
  ($expression : Expression))
  : Expression-Context
  (struct-copy expression-context $expression-context
    (public-expression-stack 
      (push
        (expression-context-public-expression-stack $expression-context)
        $expression))))

(define (expression-context-push 
  ($expression-context : Expression-Context)
  ($expression : Expression))
  : Expression-Context
  (expression-context-push-public
    (expression-context-push-private $expression-context $expression)
    $expression))
