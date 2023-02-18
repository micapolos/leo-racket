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

point:
  x 10
  y 20
do
  point.x.fixnum
  plus point.y.fixnum
do
  "x + y = "
  plus fixnum.string
