#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/expression
  leo/typed/stack)

(struct expression-stacks (
  (private : (Stackof Expression))
  (public : (Stackof Expression)))
  #:transparent
  #:type-name Expression-Stacks)

(define (expression-stacks-push-private 
  ($expression-stacks : Expression-Stacks)
  ($expression : Expression))
  : Expression-Stacks
  (struct-copy expression-stacks $expression-stacks
    (private
      (push
        (expression-stacks-private $expression-stacks)
        $expression))))

(define (expression-stacks-push-public 
  ($expression-stacks : Expression-Stacks)
  ($expression : Expression))
  : Expression-Stacks
  (struct-copy expression-stacks $expression-stacks
    (public
      (push
        (expression-stacks-public $expression-stacks)
        $expression))))

(define (expression-stacks-push 
  ($expression-stacks : Expression-Stacks)
  ($expression : Expression))
  : Expression-Stacks
  (expression-stacks-push-public
    (expression-stacks-push-private $expression-stacks $expression)
    $expression))
