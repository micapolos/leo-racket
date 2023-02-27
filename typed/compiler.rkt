#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/token
  leo/typed/type
  leo/typed/syntax-type
  leo/typed/binding2)

(define-type Processor (U SyntaxStackProcessor))

(struct syntax-stack-processor (
  (binding-stack : (Stackof Binding2))
  (syntax-stack : (Stackof Syntax))
  (parent : (Option (U SyntaxStackParent))))
  #:transparent
  #:type-name SyntaxStackProcessor)

(struct syntax-stack-processor-resolver (
  (syntax-stack-processor : SyntaxStackProcessor)
  (symbol : Symbol))
  #:transparent
  #:type-name SyntaxStackProcessorResolver)

(define (syntax-stack-processor-push 
  ($processor : SyntaxStackProcessor)
  ($syntax : Syntax)) : SyntaxStackProcessor
  (struct-copy syntax-stack-processor $processor
    (syntax-stack 
      (push (syntax-stack-processor-syntax-stack $processor) $syntax))))

(define (syntax-stack-processor-plus 
  ($processor : SyntaxStackProcessor)
  ($token : Token)) : Processor
  (cond
    ((open? $token)
      (syntax-stack-processor
        (syntax-stack-processor-binding-stack $processor)
        null
        (lambda (($syntax-stack : (Stackof Syntax)))
          (syntax-stack-processor-push
            $processor
            (syntax-with-type
              #`todo
              (tuple 
                (open-symbol $token) 
                (map 
                  syntax-type 
                  (syntax-stack-processor-syntax-stack $processor))))))))
    ((close? $token) 
      ((syntax-stack-processor-close $processor) 
        (syntax-stack-processor-syntax-stack $processor)))
    ((syntax? $token) 
      (syntax-stack-processor-push $processor $token))))

(syntax-stack-processor-plus
  (syntax-stack-processor null null (lambda (($syntax-stack : (Stackof Syntax))) (error "dupa")))
  (open `tuple))
