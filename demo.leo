#lang leo

use package base

recipe
  number.fibonacci.repeats
    number.doing
      number.less than 2
      switch
        number
        the
          number, minus 2, fibonacci
          plus: number, minus 1, fibonacci
do 42.fibonacci.time
