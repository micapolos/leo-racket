#lang leo

do require:
  racket/class
  racket/gui/base
  leo/dc
  pict

do
  interval
  gives 16

do
  pict
  gives
    jack-o-lantern 100

do
  frame
  gives new:
    frame%
    label "Hello, world!"
    width 640
    height 480

do
  canvas
  gives new:
    canvas%
    parent frame
    paint-callback function
      for: canvas dc
      giving
        dc
        invoke-preserving-transformation function giving
          do
            time
            gives
              current-inexact-monotonic-milliseconds:
              divided-by 1000.0
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
              plus 2.5
            give
              time
              times 15
              sin
              plus 2.5
          do send: dc translate -50 -50
          do draw-pict:
            pict
            dc
            0
            0
          do refresh:

do
  refresh:
  gives
    new:
      timer%
      interval interval
      just-once? true
      notify-callback function giving
        send: canvas refresh

do send: frame show true
