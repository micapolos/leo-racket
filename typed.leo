#lang leo/typed

define
  string
  plus string
  giving string
  is string-append

define
  string
  length
  giving number
  is string-length

define
  number
  plus number
  giving number
  is +

define
  number
  minus number
  giving number
  is -

define
  number
  times number
  giving number
  is *

define
  number
  string
  giving string
  is number->string

define
  pi
  giving number
  is 3.15159

"Hello, "
plus "world!"
length
plus pi
string
plus " apples."
