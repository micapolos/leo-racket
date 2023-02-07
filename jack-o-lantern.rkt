#lang leo

require:
  racket/class
  racket/gui/base
  leo/dc
  pict

variable interval 16

variable pict jack-o-lantern 100

variable frame new:
  frame%
  label "Hello, world!"
  width 640
  height 480

variable canvas new:
  canvas%
  parent frame
  paint-callback function
    for: canvas dc
    doing
      dc
      invoke-preserving-transformation function doing
        variable time
          current-inexact-milliseconds:
          divided-by 1000.0

        variable half-width
          send: canvas get-width
          divided-by 2

        variable half-height
          send: canvas get-height
          divided-by 2

        variable rotation
          time
          plus 0.5
          times 7.5
          sin
          times 0.5

        variable scale
          time
          times 15
          sin
          plus 2.5

        do:
          send: dc translate half-width half-height
          send: dc rotate rotation
          send: dc scale scale scale
          send: dc translate -50 -50
          draw-pict: pict dc 0 0
          refresh:

refresh:
does
  new:
    timer%
    interval interval
    just-once? true
    notify-callback function doing send: canvas refresh

send: frame show true
