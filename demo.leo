#lang leo

recipe
  number.increment
  does
    number
    plus 1

test
  10.increment
  equals 11
