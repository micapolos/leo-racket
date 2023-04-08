#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/type-utils)

(define (syntax-type ($syntax : Syntax)) : Type
  (define $syntax-e (syntax-e $syntax))
  (cond
    ((null? $syntax-e) (racket))
    ((symbol? $syntax-e) 
      (case $syntax-e
        ((number) number-type)
        ((int) int-type)
        ((float) float-type)
        ((text) text-type)
        ((boolean) boolean-type)
        (else (field! $syntax-e))))
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
            ((equal? $symbol `recipe) (car $structure))
            ((equal? $symbol `choice) (choice $structure))
            ((equal? $symbol `word) (word-syntax-list-type $cdr))
            ; TODO: Parse universe / generic / recursive / recurse
            (else (field $symbol $structure))))
        (else (racket))))
    (else (racket))))

(define (word-syntax-list-type ($syntax-list : (Listof Syntax))) : Type
  (word-syntax-type
    (option-or
      (top-option $syntax-list)
      (error "word requires single syntax"))))

(define (word-syntax-type ($syntax : Syntax)) : Type
  (option-or
    (and
      (symbol? (syntax-e $syntax))
      (field (syntax-e $syntax) null))
    (syntax-match-symbol-args $syntax $symbol $args
      (field $symbol (syntax-list-structure $args)))
    (error "word type expected")))

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
            ((or (equal? $symbol `giving) (equal? $symbol `doing))
              (list (arrow $structure $rhs-structure)))
            (else (push $structure (syntax-type $syntax)))))
        (else (push $structure (syntax-type $syntax)))))
    (else (push $structure (syntax-type $syntax)))))

(check-equal? (syntax-type #`()) (racket))
(check-equal? (syntax-type #`foo) (field! `foo))

(check-equal? (syntax-type #`(foo)) (field! `foo))

(check-equal? (syntax-type #`number) number-type)
(check-equal? (syntax-type #`(word number)) (field! `number))

(check-equal?
  (syntax-type #`(recipe foo bar (doing zoo)))
  (arrow (stack (field! `foo) (field! `bar)) (stack (field! `zoo))))

(check-equal?
  (syntax-type #`(choice true false))
  (choice! (field! `true) (field! `false)))

(check-equal? 
  (syntax-type #`(foo number text))
  (field `foo (stack number-type text-type)))

(check-equal? 
  (syntax-list-structure (list #`foo #`bar))
  (stack (field! `foo) (field! `bar)))

(check-equal? 
  (syntax-list-structure (list #`foo #`bar #`(giving zoo)))
  (list (arrow (stack (field! `foo) (field! `bar)) (stack (field! `zoo)))))

(check-equal?
  (syntax-list-structure (list #`foo #`bar #`(giving zoo)))
  (list (arrow (stack (field! `foo) (field! `bar)) (stack (field! `zoo)))))
