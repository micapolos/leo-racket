#lang leo

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
