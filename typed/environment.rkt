#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/maybe
  leo/typed/base
  leo/typed/testing
  (only-in racket/unsafe/ops unsafe-fx+))

(data environment 
  (namespace : Namespace)
  (namespace-procedure : (-> Namespace)))

(define (environment-with-namespace-procedure 
  ($namespace-procedure : (-> Namespace))) : Environment
  (environment ($namespace-procedure) $namespace-procedure))

(define base-environment
  (environment-with-namespace-procedure make-base-namespace))

(define (environment-with-updated-namespace
  ($environment : Environment) 
  ($fn : (-> Namespace Void)))
  : Environment
  (environment-with-namespace-procedure 
    (lambda ()
      (define $namespace ((environment-namespace-procedure $environment)))
      ($fn $namespace)
      $namespace)))

(define (environment-with-updated-current-namespace
  ($environment : Environment) 
  ($fn : (-> Void)))
  : Environment
  (environment-with-updated-namespace
    $environment
    (lambda (($namespace : Namespace))
      (parameterize ((current-namespace $namespace)) ($fn)))))

(define (environment-set
  ($environment : Environment) 
  ($symbol : Symbol) 
  ($value : Any))
  : Environment
  (environment-with-updated-current-namespace
    $environment
    (lambda () (namespace-set-variable-value! $symbol $value))))

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

(define (environment-require
  ($environment : Environment) 
  ($spec : Any))
  : Environment
  (environment-with-updated-current-namespace
    $environment
    (lambda () (namespace-require $spec))))

(check-equal?
  (environment-ref 
    (environment-require base-environment `racket/unsafe/ops)
    `unsafe-fx+)
  (just unsafe-fx+))

; -----------------------------------------------------------------------------

(define (environment-eval-values ($environment : Environment) ($sexp : Sexp)) : AnyValues
  (eval $sexp (environment-namespace $environment)))

(define (environment-eval-list ($environment : Environment) ($sexp : Sexp)) : (Listof Any)
  (call-with-values
    (lambda () (environment-eval-values $environment $sexp))
    (ann list (-> Any * (Listof Any)))))

(define (environment-eval ($environment : Environment) ($sexp : Sexp)) : Any
  (bind $list
    (environment-eval-list $environment $sexp)
    (if (null? list) (void) (car (reverse $list)))))

(check-equal?
  (environment-eval-list base-environment `(values 1 2 3))
  (list 1 2 3))

(check-equal?
  (environment-eval base-environment `(values 1 2 3))
  3)

(check-equal?
  (environment-eval base-environment `(+ 1 2))
  3)

(check-equal?
  (environment-eval 
    (environment-set base-environment `plus +)
    `(+ 1 2))
  3)

(check-equal?
  (environment-eval 
    (environment-require base-environment `racket/unsafe/ops)
    `(unsafe-fx+ 1 2))
  3)
