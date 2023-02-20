#lang leo

require racket/math

bind zero 0

bind one 0

define
  pi
  being number.pi
  is native pi

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
  string
  plus string
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
    plus "!!!"

"Hello, "
plus "world"
exclamate
do
  string
  plus " ("
  plus string.length.string
  plus ")"

numbers: 10 20
do
  numbers.first.number.string
  plus " + "
  plus numbers.second.number.string
  plus " = "
  plus
    numbers.first.number
    plus numbers.second.number
    string

point:
  center point:
    x 10.0
    y 20.0
  radius 30.0
do
  point.center.point.x.number
  plus 100.0
  plus pi.number
  string
  plus "!!!"
