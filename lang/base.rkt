#lang leo/core

do require:
  rackunit
  racket/bool
  racket/function
  racket/string
  for-syntax racket/base
  rename-in:
    racket/base
    map racket-map
    filter racket-filter

do
  (define-syntax (gives stx)
    (syntax-case stx (more)
      ((_ (name params ... (more param)) body ...)
        #`(define (name params ... . param ) body ...))
      ((_ name-and-params body ...)
        #`(define name-and-params body ...))))

do
  (define-syntax (has stx)
    (syntax-case stx ()
      ((_ name fields ...)
        #`(struct name (fields ...) #:transparent))))

do
  (define-syntax (exists stx)
    (syntax-case stx ()
      ((_ (name fields ...))
        #`(struct name (fields ...) #:transparent))))

do define: plus +
do define: minus -
do define: times *
do define: divided-by /
do define: less-than? <
do define: greater-than? >
do define: less-or-equal? <=
do define: greater-or-equal? >=

do
  any? x
  gives true

do
  true? x
  gives
    x
    equal? true

do
  any? "jajko"
  check-equal? true

do
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

do
  point
  has: x y

do
  point:
    give
      1
      plus 2
    give
      3
      plus 4
  as $point in begin
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
  as $pair in begin
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

do
  (define-syntax (giving stx)
    (syntax-case stx ()
      ((_ params ... body)
        #`(lambda (params ...) body))))

do
  (define-syntax (use stx)
    (syntax-case stx ()
      ((_ xs ...)
        #`(#%app xs ...))))

do
  give: x y
  giving minus: x y
  use: 5 3
  check-equal? 2

do
  (begin-for-syntax
    (define (expand-otherwise-stx lhs alternate)
      (let ((tmp (car (generate-temporaries `(tmp)))))
        (expand-if-stx
          lhs
          tmp
          (list #`(`otherwise #,alternate)))))
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

do
  (define-syntax (otherwise stx)
    (syntax-case stx (if true?)
      ((_ (if condition (true? consequent)) alternate)
        #`(if condition consequent alternate))
      ((_ lhs alternate)
        (expand-otherwise-stx #`lhs #`alternate))))

do
  1
  if number? "number"
  if string? "string"
  otherwise "something else"
  check-equal? "number"

do
  "foo"
  if number? "number"
  if string? "string"
  otherwise "something else"
  check-equal? "string"

do
  true
  if number? "number"
  if string? "string"
  otherwise "something else"
  check-equal? "something else"

do
  (define-syntax (leo stx)
    (syntax-case stx ()
      ((_ body)
        (let ((anchor (car (generate-temporaries `(anchor)))))
          #`(begin
            (define-namespace-anchor #,anchor)
            (parameterize
              ((current-namespace (namespace-anchor->namespace #,anchor)))
              (syntax->datum (expand (datum->syntax #f (quote body))))))))))

do define: join string-append

do
  "Hello, "
  join "world!"
  check-equal? "Hello, world!"

do
  push: list value
  gives cons: value list

do define: using curryr

do
  map: list fn
  gives racket-map: fn list

do
  filter: list fn
  gives racket-filter: fn list

do
  string
  in-round-brackets
  gives join: "(" string ")"

do
  more strings
  comma-separated
  gives
    strings
    string-join ", "

do
  comma-separated: "a" "b" "c"
  check-equal? "a, b, c"
