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
          (define $type-stack (syntax-list-type-stack $cdr))
          (define $type (single $type-stack))
          (cond
            ((and (equal? $symbol `a) $type) (a $type))
            (else (field $symbol $type-stack))))
        (else (racket (syntax->datum $syntax)))))
    (else (racket (syntax->datum $syntax)))))

(define (syntax-list-type-stack ($syntax-list : (Listof Syntax))) : (Stackof Type)
  (foldl
    (lambda (($syntax : Syntax) ($type-stack : (Stackof Type)))
      (type-stack+syntax $type-stack $syntax))
    null
    $syntax-list))

(define (type-stack+syntax 
  ($type-stack : (Stackof Type))
  ($syntax : Syntax)) 
  : (Stackof Type)
  (define $syntax-e (syntax-e $syntax))
  (cond
    ((null? $syntax-e) 
      (push $type-stack (syntax-type $syntax)))
    ((list? $syntax-e)
      (define $list $syntax-e)
      (define $car (car $list))
      (define $car-syntax-e (syntax-e $car))
      (define $cdr (cdr $list))
      (cond
        ((symbol? $car-syntax-e) 
          (define $symbol $car-syntax-e)
          (define $rhs-type-stack (syntax-list-type-stack $cdr))
          (define $rhs-type (single $rhs-type-stack))
          (cond
            ((and (equal? $symbol `giving) $rhs-type)
              (list (arrow $type-stack $rhs-type)))
            (else (push $type-stack (syntax-type $syntax)))))
        (else (push $type-stack (syntax-type $syntax)))))
    (else (push $type-stack (syntax-type $syntax)))))

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
  (syntax-list-type-stack (list #`foo #`bar))
  (stack (racket `foo) (racket `bar)))

(check-equal? 
  (syntax-list-type-stack (list #`foo #`bar #`(giving zoo)))
  (list (arrow (stack (racket `foo) (racket `bar)) (racket `zoo))))
