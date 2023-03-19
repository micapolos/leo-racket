#lang leo

circle
  point
    x 10
    y 20
    z 30
  radius
    "foo"
    plus "bar"
do
  circle.point
  do
    point.x.number
    plus point.y.number
racket
