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

(define-syntax (variable stx)
  (syntax-case stx (more)
    ((_ (name body ...))
      #`(define name body ...))))

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

the:
  variable plus +
  variable minus -
  variable times *
  variable divided-by /
  variable less-than? <
  variable greater-than? >
  variable less-or-equal? <=
  variable greater-or-equal? >=

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

variable test
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

point
has: x y

point:
  the
    1
    plus 2
  the
    3
    plus 4
as $point in begin
  $point
  check-equal? point: 3 7

  $point
  point?
  check-equal? true

  "foo"
  point?
  check-equal? false

  $point
  point-x
  check-equal? 3

  $point
  point-y
  check-equal? 7

lhs
pair-to rhs
exists

1
plus 2
pair-to
  3
  plus 4
as $pair in begin
  $pair
  check-equal?
    3
    pair-to 7

  $pair
  pair-to?
  check-equal? true

  "foo"
  pair-to?
  check-equal? false

  $pair
  pair-to-lhs
  check-equal? 3

  $pair
  pair-to-rhs
  check-equal? 7

(define-syntax (function stx)
  (syntax-case stx (doing for)
    ((_ (doing (for params ...) body ...))
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
  for: x y
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

(define-syntax (otherwise stx)
  (syntax-case stx (if true?)
    ((_ (if condition (true? consequent)) alternate)
      #`(if condition consequent alternate))
    ((_ lhs alternate)
      (expand-otherwise-stx #`lhs #`alternate))))

1
if number? "number"
if string? "string"
otherwise "something else"
check-equal? "number"

"foo"
if number? "number"
if string? "string"
otherwise "something else"
check-equal? "string"

true
if number? "number"
if string? "string"
otherwise "something else"
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

variable join string-append

"Hello, "
join "world!"
check-equal? "Hello, world!"

push: list value
does cons: value list

variable applying curryr

map: list fn
does racket-map: fn list

list: 1 2 3
map applying: - 1
check-equal? list: 0 1 2

filter: list fn
does racket-filter: fn list

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
