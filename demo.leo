#lang leo

use package base

10
countdown
repeat
  text
  doing
    number
    equals 0
    switch
      "OK"
      the
        number
        minus 1
        countdown
