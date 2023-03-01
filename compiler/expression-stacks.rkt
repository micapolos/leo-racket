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

(define (expression-stacks-private-push
  ($expression-stacks : Expression-Stacks)
  ($expression : Expression))
  : Expression-Stacks
  (struct-copy expression-stacks $expression-stacks
    (private
      (push
        (expression-stacks-private $expression-stacks)
        $expression))))


(define (expression-stacks-private-push-stack
  ($expression-stacks : Expression-Stacks)
  ($expression-stack : (Stackof Expression)))
  : Expression-Stacks
  (struct-copy expression-stacks $expression-stacks
    (private 
      (append 
        $expression-stack
        (expression-stacks-private $expression-stacks)))))

(define (expression-stacks-push-stack
  ($expression-stacks : Expression-Stacks)
  ($expression-stack : (Stackof Expression)))
  : Expression-Stacks
  (expression-stacks
    (append 
      $expression-stack
      (expression-stacks-private $expression-stacks))
    (append 
      $expression-stack
      (expression-stacks-public $expression-stacks))))

(define (expression-stacks-public-push
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
  (expression-stacks-public-push
    (expression-stacks-private-push $expression-stacks $expression)
    $expression))

(define (expression-stacks-begin
  ($expression-stacks : Expression-Stacks))
  : Expression-Stacks
  (struct-copy expression-stacks $expression-stacks
    (public null)))
