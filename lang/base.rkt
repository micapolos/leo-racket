#lang infix/leo/core

do require:
  rackunit
  for-syntax racket/base

racket
  (define-syntax (does stx)
    (syntax-case stx ()
      ((_ params body ...)
        #`(define params body ...))))

racket
  (define-syntax (plus stx)
    (syntax-case stx ()
      ((_ lhs rhs)
        #`(+ lhs rhs))))

do
  2
  plus 3
  check-equal? 5

racket
  (define-syntax (minus stx)
    (syntax-case stx ()
        ((_ lhs rhs)
          #`(- lhs rhs))))

do
  5
  minus 3
  check-equal? 2

racket
  (define-syntax (times stx)
    (syntax-case stx ()
        ((_ lhs rhs)
          #`(* lhs rhs))))

do
  2
  times 3
  check-equal? 6

racket
  (define-syntax (less-than? stx)
    (syntax-case stx ()
        ((_ lhs rhs)
          #`(< lhs rhs))))

racket
  (define-syntax (grater-than? stx)
    (syntax-case stx ()
        ((_ lhs rhs)
          #`(> lhs rhs))))

do
  true
  does #t

do
  false
  does #f

do
  2
  less-than? 3
  check-equal? true

do
  any? x
  does #t

do
  true? x
  does
    x
    equal? #t

do
  false? x
  does
    x
    equal? #f

do
  any? "jajko"
  check-equal? true

racket
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

racket
  (define-syntax (has stx)
    (syntax-case stx (in)
      ((_ name fields ...)
        #`(struct name (fields ...) #:transparent))))

do begin
  do
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

racket
  (define-syntax (exists stx)
    (syntax-case stx ()
      ((_ (name fields ...))
        #`(has name fields ...))))

do begin
  do
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

racket
  (define-syntax (doing stx)
    (syntax-case stx ()
      ((_ params ... body)
        #`(lambda (params ...) body))))

racket
  (define-syntax (apply stx)
    (syntax-case stx ()
      ((_ xs ...) 
        #`(#%app xs ...))))

do
  do: x y
  doing minus: x y
  apply: 5 3
  check-equal? 2

racket
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

racket
  (define-syntax (else stx)
    (syntax-case stx ()
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
