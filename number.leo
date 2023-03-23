#lang leo

racket
  =
  of recipe
    number
    equals number
    doing boolean

racket
  <
  of recipe
    number
    less than number
    doing boolean

racket
  >
  of recipe
    number
    greater than number
    doing boolean

racket
  <=
  of recipe
    number
    less or equal to number
    doing boolean

racket
  >=
  of recipe
    number
    greater or equal to number
    doing boolean

racket
  +
  of recipe
    number
    plus number
    doing number

racket
  -
  of recipe
    number
    minus number
    doing number

racket
  *
  of recipe
    number
    times number
    doing number

racket
  /
  of recipe
    number
    divided by number
    doing number

racket
  add1
  of recipe
    number.increment
    doing number

racket
  sub1
  of recipe
    number.increment
    doing number

racket
  sin
  of recipe
    number
    sinus
    doing number

racket
  cos
  of recipe
    number
    cosinus
    doing number

racket
  sqrt
  of recipe
    number
    square root
    doing number

pi racket
  (local racket/math pi)
  of number

racket
  number->string
  of recipe
    number.text
    doing text
