#lang leo/core

require:
  do
    racket/base
    rename-in:
      define racket-define
  rackunit
  for-syntax racket/base

(define-syntax (define stx)
  (syntax-case stx (does has exists)
    ((_ (does params body ...))
      #`(racket-define params body ...))
    ((_ (has name fields ...))
      #`(struct name (fields ...) #:transparent))
    ((_ (exists (name fields ...)))
      #`(struct name (fields ...) #:transparent))
    ((_ (params body ...))
      #`(racket-define params body ...))))

(define-syntax (plus stx)
  (syntax-case stx ()
    ((_ lhs rhs)
      #`(+ lhs rhs))))

do
  2
  plus 3
  check-equal? 5

(define-syntax (minus stx)
  (syntax-case stx ()
    ((_ lhs rhs)
      #`(- lhs rhs))))

do
  5
  minus 3
  check-equal? 2

(define-syntax (times stx)
  (syntax-case stx ()
    ((_ lhs rhs)
      #`(* lhs rhs))))

do
  2
  times 3
  check-equal? 6

(define-syntax (less-than? stx)
  (syntax-case stx ()
    ((_ lhs rhs)
      #`(< lhs rhs))))

(define-syntax (grater-than? stx)
  (syntax-case stx ()
    ((_ lhs rhs)
      #`(> lhs rhs))))

define
  true
  does #t

define
  false
  does #f

do
  2
  less-than? 3
  check-equal? true

define
  any? x
  does #t

define
  true? x
  does
    x
    equal? #t

define
  false? x
  does
    x
    equal? #f

do
  any? "jajko"
  check-equal? true

(define-syntax (as stx)
  (syntax-case stx (in)
    ((_ lhs (symbol (in body ...)))
      (quasisyntax (let ((symbol lhs)) body ...)))))

do
  3
  plus 4
  as number in
    number
    plus number
  check-equal? 14

do begin
  define
    point
    has: x y
  do
    point:
      do
        1
        plus 2
      do
        3
        plus 4
    as $point in
      do
        $point
        check-equal? point: 3 7
      do
        $point
        point?
        check-equal? true
      do
        "foo"
        point?
        check-equal? false
      do
        $point
        point-x
        check-equal? 3
      do
        $point
        point-y
        check-equal? 7

do begin
  define
    lhs
    pair-to rhs
    exists
  do
    1
    plus 2
    pair-to
      3
      plus 4
    as $pair in
      do
        $pair
        check-equal?
          3
          pair-to 7
      do
        $pair
        pair-to?
        check-equal? true
      do
        "foo"
        pair-to?
        check-equal? false
      do
        $pair
        pair-to-lhs
        check-equal? 3
      do
        $pair
        pair-to-rhs
        check-equal? 7

(define-syntax (doing stx)
  (syntax-case stx ()
    ((_ params ... body)
      #`(lambda (params ...) body))))

(define-syntax (apply stx)
  (syntax-case stx ()
    ((_ xs ...) 
      #`(#%app xs ...))))

do
  do: x y
  doing minus: x y
  apply: 5 3
  check-equal? 2

(begin-for-syntax
  (define (expand-else-stx lhs alternate)
    (let ((tmp (car (generate-temporaries `(tmp)))))
      (expand-if-stx
        lhs
        tmp
        (list #`(`else #,alternate)))))
  (define (expand-if-stx if-stx tmp tail)
    (syntax-case if-stx (if)
      ((if expression (predicate consequent))
        (expand-if-stx
          #`expression
          tmp
          (cons #`((predicate #,tmp) consequent) tail)))
      (lhs
        #`(let ((#,tmp lhs))
          (cond #,@tail))))))

(define-syntax (else stx)
  (syntax-case stx (if true?)
    ((_ (if condition (true? consequent)) alternate)
      #`(if condition consequent alternate))
    ((_ lhs alternate)
      (expand-else-stx #`lhs #`alternate))))

do
  1
  if number? "number"
  if string? "string"
  else "something else"
  check-equal? "number"

do
  "foo"
  if number? "number"
  if string? "string"
  else "something else"
  check-equal? "string"

do
  true
  if number? "number"
  if string? "string"
  else "something else"
  check-equal? "something else"
