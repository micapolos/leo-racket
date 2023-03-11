#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils)

(define (syntax-type ($syntax : Syntax)) : Type
  (define $syntax-e (syntax-e $syntax))
  (cond
    ((null? $syntax-e) (racket))
    ((symbol? $syntax-e) 
      (case $syntax-e
        ((boolean) boolean-type)
        ((number) number-type)
        ((int) int-type)
        ((float) float-type)
        ((text) text-type)
        (else (null-field $syntax-e))))
    ((list? $syntax-e)
      (define $list $syntax-e)
      (define $car (car $list))
      (define $cdr (cdr $list))
      (define $car-syntax-e (syntax-e $car))
      (cond
        ((symbol? $car-syntax-e)
          (define $symbol $car-syntax-e)
          (define $structure (syntax-list-structure $cdr))
          (define $type (single $structure))
          (cond
            ; TODO: Parse universe / generic / recursive / recurse
            (else (field $symbol $structure))))
        (else (racket))))
    (else (racket))))

(define (syntax-list-structure ($syntax-list : (Listof Syntax))) : Structure
  (foldl
    (lambda (($syntax : Syntax) ($structure : Structure))
      (structure+syntax $structure $syntax))
    null
    $syntax-list))

(define (structure+syntax 
  ($structure : Structure)
  ($syntax : Syntax)) 
  : Structure
  (define $syntax-e (syntax-e $syntax))
  (cond
    ((null? $syntax-e) 
      (push $structure (syntax-type $syntax)))
    ((list? $syntax-e)
      (define $list $syntax-e)
      (define $car (car $list))
      (define $car-syntax-e (syntax-e $car))
      (define $cdr (cdr $list))
      (cond
        ((symbol? $car-syntax-e) 
          (define $symbol $car-syntax-e)
          (define $rhs-structure (syntax-list-structure $cdr))
          (cond
            ((equal? $symbol `giving)
              (list (arrow $structure $rhs-structure)))
            (else (push $structure (syntax-type $syntax)))))
        (else (push $structure (syntax-type $syntax)))))
    (else (push $structure (syntax-type $syntax)))))

(check-equal? (syntax-type #`()) (racket))
(check-equal? (syntax-type #`foo) (null-field `foo))

(check-equal? (syntax-type #`(foo)) (null-field `foo))

(check-equal? 
  (syntax-type #`(foo number text))
  (field `foo (stack number-type text-type)))

(check-equal? 
  (syntax-list-structure (list #`foo #`bar))
  (stack (null-field `foo) (null-field `bar)))

(check-equal? 
  (syntax-list-structure (list #`foo #`bar #`(giving zoo)))
  (list (arrow (stack (null-field `foo) (null-field `bar)) (stack (null-field `zoo)))))
