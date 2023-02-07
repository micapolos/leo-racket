#lang leo

do require:
  racket/class
  racket/gui/base
  leo/dc
  pict

do
  interval
  gives 10

do
  dt
  gives
    1
    divided-by 60

do
  time
  gives 0.0

do
  pict
  gives
    jack-o-lantern 100
    freeze

new:
  frame%
  label "Hello, world!"
  width 640
  height 480
as frame in
  new:
    canvas%
    parent frame
    paint-callback function
      for: canvas dc
      giving
        dc
        invoke-preserving-transformation function giving
          set!:
            time
            plus: time dt
          do send:
            dc
            translate
            give
              send: canvas get-width
              divided-by 2
            give
              send: canvas get-height
              divided-by 2
          do send:
            dc
            rotate
            give
              time
              plus 0.5
              times 7.5
              sin
              times 0.5
          do send:
            dc
            scale
            give
              time
              times 15
              sin
              plus 3
            give
              time
              times 15
              sin
              plus 3
          do send: dc translate -50 -50
          do draw-pict:
            pict
            dc
            0
            0
  as canvas in
    do new:
      timer%
      interval interval
      notify-callback function giving
        send: canvas refresh-now
  give: frame
send: show true
