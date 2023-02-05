#lang leo

define
  number
  fibonacci
  does
    number
    less-than? 2
    if true? number
    else
      number
      minus 2
      fibonacci
      plus
        number
        minus 1
        fibonacci

do
  42
  fibonacci
