#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/maybe
  leo/typed/base
  leo/typed/testing)

(data environment 
  (namespace : Namespace)
  (namespace-procedure : (-> Namespace)))

(define (environment-with-namespace-procedure 
  ($namespace-procedure : (-> Namespace))) : Environment
  (environment ($namespace-procedure) $namespace-procedure))

(define base-environment
  (environment-with-namespace-procedure make-base-namespace))

(define (environment-set
  ($environment : Environment) 
  ($symbol : Symbol) 
  ($value : Any))
  : Environment
  (environment-with-namespace-procedure 
    (lambda ()
      (define $namespace ((environment-namespace-procedure $environment)))
      (namespace-set-variable-value! $symbol $value #f $namespace)
      $namespace)))

(define namespace-absent-value (gensym))

(define (environment-ref
  ($environment : Environment) 
  ($symbol : Symbol))
  : (Maybe Any)
  (bind $value
    (namespace-variable-value $symbol
      #t 
      (lambda () namespace-absent-value)
      (environment-namespace $environment))
    (if (equal? $value namespace-absent-value) #f (just $value))))

(check-equal? (environment-ref base-environment `foo) #f)

(check-equal? 
  (environment-ref (environment-set base-environment `foo "foo") `foo) 
  (just "foo"))

(check-equal?
  (environment-ref (environment-set base-environment `foo "foo") `bar) 
  #f)

(define (environment-eval-values ($environment : Environment) ($any : Any)) : AnyValues
  (eval $any (environment-namespace $environment)))

; (define (environment-eval ($environment : Environment) ($any : Any))
;   (call-with-values
;     (lambda () (environment-eval-values $environment $any))
;     list))
