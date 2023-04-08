#lang leo

use package number
use package text

recipe
  number, fibonacci
  repeats
    number
    doing
      number
      less than 2
      switch
        take number
        take
          number, minus 2, fibonacci
          plus: number, minus 1, fibonacci
do: 42, fibonacci, time
