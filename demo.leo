#lang leo

require: racket/unsafe/ops

define
  fixnum
  plus fixnum
  giving fixnum
  does native unsafe-fx+

define
  string
  plus string
  giving string
  does native string-append

define
  string.length
  giving fixnum
  does native string-length

define
  fixnum.string
  giving string
  does native number->string

"Hello, "
plus "world!"
do
  string
  plus " ("
  plus string.length.string
  plus ")"

numbers: 10 20
do
  numbers.first.fixnum.string
  plus " + "
  plus numbers.second.fixnum.string
  plus " = "
  plus
    numbers.first.fixnum
    plus numbers.second.fixnum
    string

point:
  center point:
    x 10
    y 20
  radius 30
do
  point.center.point.x.fixnum
  plus 100
