#lang leo

require:
  racket/base
  racket/math

define
  number
  plus number
  giving number
  does native +

define
  number
  minus number
  giving number
  does native -

define
  number
  times number
  giving number
  does native *

define
  number.string
  giving string
  does native number->string

define
  pi
  being number.pi
  is native pi

bind zero 0

bind one 1

bind two
  one.number
  plus one.number

zero.number

one.number

two.number

define
  number.squared
  does
    squared.number
    times squared.number

16
squared

define
  circle radius number
  area
  does
    pi.number
    times area.circle.radius.number.squared

circle radius 10
area

define
  string
  append string
  giving string
  does native string-append

define
  string.length
  giving number
  does native string-length

define
  string.exclamate
  does
    exclamate.string
    append "!!!"

"Hello"
exclamate

"Hello, "
append "world"
exclamate
do
  string
  append " ("
  append string.length.string
  append ")"

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

circle:
  center point:
    x 10.0
    y 20.0
  radius 30.0
do
  circle.center.point.x.number
  plus 100.0
  plus pi.number
  string.exclamate

; lisp syntax
(plus 1 2)
(append "Hello, " "world!")
(circle (center (point 10 20)) (radius 30))
