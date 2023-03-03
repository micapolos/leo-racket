#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/racket)

(define (syntax-type ($syntax : Syntax)) : Type
  (define $syntax-e (syntax-e $syntax))
  (cond
    ((null? $syntax-e) 
      (racket $syntax-e))
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
            ((and (equal? $symbol `a) $type) (a $type))
            (else (field $symbol $structure))))
        (else (racket (syntax->datum $syntax)))))
    (else (racket (syntax->datum $syntax)))))

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

(check-equal? (syntax-type #`()) (racket `()))
(check-equal? (syntax-type #`foo) (racket `foo))

(check-equal? (syntax-type #`(a)) (field `a null))
(check-equal? (syntax-type #`(a foo)) (a (racket `foo)))
(check-equal? (syntax-type #`(a foo bar)) (field `a (stack (racket `foo) (racket `bar))))

(check-equal? (syntax-type #`(foo)) (field `foo null))

(check-equal? 
  (syntax-type #`(foo number string))
  (field `foo (stack (racket `number) (racket `string))))

(check-equal? 
  (syntax-list-structure (list #`foo #`bar))
  (stack (racket `foo) (racket `bar)))

(check-equal? 
  (syntax-list-structure (list #`foo #`bar #`(giving zoo)))
  (list (arrow (stack (racket `foo) (racket `bar)) (stack (racket `zoo)))))
