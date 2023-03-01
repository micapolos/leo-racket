#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/expression
  leo/typed/stack)

(struct expression-stacks (
  (private : (Stackof Expression))
  (public : (Stackof Expression)))
  #:transparent
  #:type-name Expression-Context)

(define (expression-stacks-push-private 
  ($expression-stacks : Expression-Context)
  ($expression : Expression))
  : Expression-Context
  (struct-copy expression-stacks $expression-stacks
    (private
      (push
        (expression-stacks-private $expression-stacks)
        $expression))))

(define (expression-stacks-push-public 
  ($expression-stacks : Expression-Context)
  ($expression : Expression))
  : Expression-Context
  (struct-copy expression-stacks $expression-stacks
    (public
      (push
        (expression-stacks-public $expression-stacks)
        $expression))))

(define (expression-stacks-push 
  ($expression-stacks : Expression-Context)
  ($expression : Expression))
  : Expression-Context
  (expression-stacks-push-public
    (expression-stacks-push-private $expression-stacks $expression)
    $expression))
