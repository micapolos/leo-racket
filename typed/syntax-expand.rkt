#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/typed
  leo/typed/types
  leo/typed/syntax-typed
  leo/typed/syntax-match
  leo/typed/typed-syntax
  leo/typed/testing)

(define (syntax-expand ($syntax : Syntax)) : (Option Syntax)
  (bind $syntax-e (syntax-e $syntax)
    (or
      (and (string? $syntax-e) (string-typed-syntax $syntax-e))
      (and (number? $syntax-e) (number-typed-syntax $syntax-e))
      (syntax-symbol-match-arg $syntax `boolean $arg
        (bind $syntax-e (syntax-e $arg)
          (or 
            (and (equal? $syntax-e `true) (boolean-typed-syntax #t))
            (and (equal? $syntax-e `false) (boolean-typed-syntax #f)))))
      (syntax-symbol-match-arg $syntax `fixnum $arg
        (bind $syntax-e (syntax-e $arg)
          (and (fixnum? $syntax-e) (fixnum-typed-syntax $syntax-e))))
      (syntax-symbol-match-arg $syntax `flonum $arg
        (bind $syntax-e (syntax-e $arg)
          (and (flonum? $syntax-e) (flonum-typed-syntax $syntax-e)))))))

(check-equal? 
  (option-map (syntax-expand #`3.14) syntax-typed-datum)
  (typed 3.14 number-type))

(check-equal? 
  (option-map (syntax-expand #`"foo") syntax-typed-datum)
  (typed "foo" string-type))

(check-equal? 
  (option-map (syntax-expand #`(fixnum 1)) syntax-typed-datum)
  (typed 1 fixnum-type))

(check-equal? 
  (option-map (syntax-expand #`(flonum 3.14)) syntax-typed-datum)
  (typed 3.14 flonum-type))

(check-equal? 
  (option-map (syntax-expand #`(boolean true)) syntax-typed-datum)
  (typed #t boolean-type))

(check-equal? 
  (option-map (syntax-expand #`(boolean false)) syntax-typed-datum)
  (typed #f boolean-type))
