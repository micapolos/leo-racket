#lang leo

require leo/base

any
  number
  fibonacci
  giving number
does recursively
  number
  less-than 2
  then number
  else
    number
    minus 2
    fibonacci
    plus
      number
      minus 1
      fibonacci

10
fibonacci
check-equals 55
