#lang leo/typed

define
  string
  plus string
  giving string
  is native string-append

define
  string
  length
  giving number
  is native string-length

define
  number
  plus number
  giving number
  is native +

define
  number
  minus number
  giving number
  is native -

define
  number
  times number
  giving number
  is native *

define
  number
  string
  giving string
  is native number->string

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
