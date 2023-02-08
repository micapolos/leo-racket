#lang leo

require:
  racket/class
  racket/gui/base
  leo/dc
  pict

define interval 16

define pict jack-o-lantern 100

define frame new:
  frame%
  label "Hello, world!"
  width 640
  height 480

define canvas new:
  canvas%
  parent frame
  paint-callback function
    for: canvas dc
    doing
      dc
      invoke-preserving-transformation function doing
        define time
          current-inexact-milliseconds:
          divided-by 1000.0

        define half-width
          send: canvas get-width
          divided-by 2

        define half-height
          send: canvas get-height
          divided-by 2

        define rotation
          time
          plus 0.5
          times 7.5
          sin
          times 0.5

        define scale
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
          do
            send: frame is-shown?
            if true? refresh:
            else void:

refresh:
does new:
  timer%
  interval interval
  just-once? true
  notify-callback function doing send: canvas refresh

send: frame show true
