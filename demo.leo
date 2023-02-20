#lang leo

require:
  racket/unsafe/ops
  racket/math

define
  pi
  being flonum.pi
  is native pi

define
  fixnum
  plus fixnum
  giving fixnum
  does native unsafe-fx+

define
  fixnum
  minus fixnum
  giving fixnum
  does native unsafe-fx-

define
  fixnum
  times fixnum
  giving fixnum
  does native unsafe-fx*

define
  fixnum.string
  giving string
  does native number->string

define
  flonum
  plus flonum
  giving flonum
  does native unsafe-fl+

define
  flonum
  minus flonum
  giving flonum
  does native unsafe-fl-

define
  flonum
  times flonum
  giving flonum
  does native unsafe-fl*

define
  flonum.string
  giving string
  does native number->string

define
  flonum.fixnum
  giving fixnum
  does native unsafe-fl->fx

define
  fixnum.flonum
  giving flonum
  does native unsafe-fx->fl

define
  string
  plus string
  giving string
  does native string-append

define
  string.length
  giving fixnum
  does native string-length

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
    x 10.0
    y 20.0
  radius 30.0
do
  point.center.point.x.flonum
  plus 100.0
  plus pi.flonum
  string
  plus "!!!"
