#lang leo

use package base

recipe
  number, take fibonacci
  repeats: number, doing
    number, less than 2, switch
      number
      the
        number, minus 2, take fibonacci
        plus: number, minus 1, take fibonacci
do: 42, take fibonacci, time
