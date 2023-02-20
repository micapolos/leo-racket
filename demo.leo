#lang leo

require:
  racket/unsafe/ops
  racket/math

define
  fixnum
  plus fixnum
  giving fixnum
  does native unsafe-fx+

fixnum 2
plus fixnum 3
check-equal? fixnum 5

define
  fixnum
  minus fixnum
  giving fixnum
  does native unsafe-fx-

fixnum 5
minus fixnum 3
check-equal? fixnum 2

define
  fixnum
  times fixnum
  giving fixnum
  does native unsafe-fx*

fixnum 2
times fixnum 3
check-equal? fixnum 6

define
  flonum
  plus flonum
  giving flonum
  does native unsafe-fl+

flonum 2.25
plus flonum 3.5
check-equal? flonum 5.75

define
  flonum
  minus flonum
  giving flonum
  does native unsafe-fl-

flonum 5.75
minus flonum 3.5
check-equal? flonum 2.25

define
  flonum
  times flonum
  giving flonum
  does native unsafe-fl*

flonum 2.5
times flonum 1.25
check-equal? flonum 3.125

define
  number
  plus number
  giving number
  does native +

2
plus 3
check-equal? 5

define
  number
  minus number
  giving number
  does native -

5
minus 3
check-equal? 2

define
  number
  times number
  giving number
  does native *

2
times 3
check-equal? 6

define
  number.string
  giving string
  does native number->string

128
string
check-equal? "128"

define
  pi
  being number.pi
  is native pi

bind zero 0

zero.number
check-equal? 0

bind one 1

one.number
check-equal? 1

bind two
  one.number
  plus one.number

two.number
check-equal? 2

define
  number.squared
  does
    squared.number
    times squared.number

16
squared
check-equal? 256

define
  circle radius number
  area
  does
    pi.number
    times area.circle.radius.number.squared

circle radius 10
area
check-equal?
  pi.number
  times 100

define
  string
  append string
  giving string
  does native string-append

"Hello, "
append "world!"
check-equal? "Hello, world!"

define
  string.length
  giving number
  does native string-length

"foo"
length
check-equal? 3

define
  string.exclamate
  does
    exclamate.string
    append "!!!"

"Hello"
exclamate
check-equal? "Hello!!!"

"Hello, "
append "world"
exclamate
do
  string
  append " ("
  append string.length.string
  append ")"
check-equal? "Hello, world!!! (15)"

numbers: 10 20
do
  numbers.first.number.string
  append " + "
  append numbers.second.number.string
  append " = "
  append
    numbers.first.number
    plus numbers.second.number
    string
check-equal? "10 + 20 = 30"

number
double
doing
  double.number
  plus double.number
apply
  10
  double
check-equal? 20

; racket syntax
(check-equal? (plus 1 2) 3)
(check-equal? (append "Hello, " "world!") "Hello, world!")
