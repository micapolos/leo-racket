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
    number
    times number

16
squared
check-equal? 256

define
  circle radius number
  area
  does
    pi.number
    times circle.radius.number.squared

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
    string
    append "!!!"

"Hello"
exclamate
check-equal? "Hello!!!"

; local binding
"Hello, "
append "world"
exclamate
do
  string
  append " ("
  append string.length.string
  append ")"
check-equal? "Hello, world!!! (15)"

; local bindings
the: 10 "apples"
do
  number.string
  append " "
  append string
check-equal? "10 apples"

; function without arguments
doing
  2
  plus 2
apply
check-equal? 4

; function with one argument
number
doing
  number
  plus 1
apply 10
check-equal? 11

; function with two arguments
the: string number
doing
  string
  append " ("
  append number.string
  append ")"
apply: "Hello" 10
check-equal? "Hello (10)"
