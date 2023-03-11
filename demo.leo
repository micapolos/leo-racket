#lang leo

circle
  radius 10
  center point
    x 20
    y 30
do
  get: circle center point x number
  add get: circle radius number
  get text
  get length
