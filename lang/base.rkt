#lang leo/core

require:
  rackunit
  racket/bool
  racket/function
  racket/string
  for-syntax racket/base
  rename-in:
    racket/base
    map racket-map
    filter racket-filter
    define racket-define
    else racket-else

(define-syntax (define stx)
  (syntax-case stx ()
    ((_ (name body ))
      #`(define name body))
    ((_ body ...)
      #`(racket-define body ...))))

(define-syntax (does stx)
  (syntax-case stx (more)
    ((_ (name params ... (more param)) body ...)
      #`(define (name params ... . param ) body ...))
    ((_ name-and-params body ...)
      #`(define name-and-params body ...))))

(define-syntax (has stx)
  (syntax-case stx ()
    ((_ name fields ...)
      #`(struct name (fields ...) #:transparent))))

(define-syntax (exists stx)
  (syntax-case stx ()
    ((_ (name fields ...))
      #`(struct name (fields ...) #:transparent))))

do:
  define plus +
  define minus -
  define times *
  define divided-by /
  define less-than? <
  define greater-than? >
  define less-or-equal? <=
  define greater-or-equal? >=

any? x
does true

true? x
does
  x
  equal? true

any? "jajko"
check-equal? true

(define-syntax (as stx)
  (syntax-case stx (in)
    ((_ lhs (symbol (in body ...)))
      (quasisyntax (let ((symbol lhs)) body ...)))))

begin
  define test
    1
    plus 2

  test
  check-equal? 3

3
plus 4
as number in
  number
  plus number
check-equal? 14

begin
  vector
  has: x y

  vector:
    do
      1
      plus 2
    do
      3
      plus 4
  as v in begin
    v
    check-equal? vector: 3 7

    v
    vector?
    check-equal? true

    "foo"
    vector?
    check-equal? false

    v
    vector-x
    check-equal? 3

    v
    vector-y
    check-equal? 7

begin
  lhs
  arrow-to rhs
  exists

  1
  plus 2
  arrow-to
    3
    plus 4
  as arrow in begin
    arrow
    check-equal?
      3
      arrow-to 7

    arrow
    arrow-to?
    check-equal? true

    "foo"
    arrow-to?
    check-equal? false

    arrow
    arrow-to-lhs
    check-equal? 3

    arrow
    arrow-to-rhs
    check-equal? 7

(define-syntax (function stx)
  (syntax-case stx (doing from)
    ((_ (doing (from params ...) body ...))
      #`(lambda (params ...) body ...))
    ((_ (doing body ...))
      #`(lambda () body ...))))

(define-syntax (invoke stx)
  (syntax-case stx ()
    ((_ xs ...)
      #`(#%app xs ...))))

function doing 3
invoke
check-equal? 3

function
  from: x y
  doing minus: x y
invoke: 3 2
check-equal? 1

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

(define-syntax (else stx)
  (syntax-case stx (if true?)
    ((_ (if condition (true? consequent)) alternate)
      #`(if condition consequent alternate))
    ((_ lhs alternate)
      (expand-otherwise-stx #`lhs #`alternate))
    ((_ body ...)
      #`(racket-else body ...))))

1
if number? "number"
if string? "string"
else "something else"
check-equal? "number"

"foo"
if number? "number"
if string? "string"
else "something else"
check-equal? "string"

true
if number? "number"
if string? "string"
else "something else"
check-equal? "something else"

(define-syntax (leo stx)
  (syntax-case stx ()
    ((_ body)
      (let ((anchor (car (generate-temporaries `(anchor)))))
        #`(begin
          (define-namespace-anchor #,anchor)
          (parameterize
            ((current-namespace (namespace-anchor->namespace #,anchor)))
            (syntax->datum (expand-once (datum->syntax #f (quote body))))))))))

define join string-append

"Hello, "
join "world!"
check-equal? "Hello, world!"

push: list value
does cons: value list

define applying curryr

map: list function
does racket-map: function list

list: 1 2 3
map applying: - 1
check-equal? list: 0 1 2

filter: list function
does racket-filter: function list

initial
fold: list f
does foldl:
  function
    from: item folded
    doing invoke: f folded item
  initial
  list

"Items: "
fold:
  list: "apple, " "banana, " "pear."
  string-append
check-equal? "Items: apple, banana, pear."

list: "foo" 2 "bar" 4
filter applying string?
check-equal? list: "foo" "bar"

string
in-round-brackets
does join: "(" string ")"

more strings
comma-separated
does
  strings
  string-join ", "

comma-separated: "a" "b" "c"
check-equal? "a, b, c"

x
flmod y
does
  x
  minus
    x
    divided-by y
    floor
    times y

4.75
flmod 3.25
check-equal? 1.5
