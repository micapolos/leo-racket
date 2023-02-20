#lang leo

require:
  racket/base
  racket/math

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

; racket syntax
(check-equal? (plus 1 2) 3)
(check-equal? (append "Hello, " "world!") "Hello, world!")
