#lang leo/typed

define
  string
  plus string
  doing string
  is string-append

define
  string
  length
  doing number
  is string-length

define
  number
  plus number
  doing number
  is +

define
  number
  minus number
  doing number
  is -

define
  number
  times number
  doing number
  is *

define
  number
  string
  doing string
  is number->string

define
  pi
  doing number
  is pi
