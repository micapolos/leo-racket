#lang leo

recipe
  number.increment
  does
    number
    plus 1

  number.decrement
  does
    number
    minus 1
do
  10
  increment
  decrement
