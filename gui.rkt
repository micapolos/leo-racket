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
    giving
      dc
      invoke-preserving-transformation function giving:
        variable time
          current-inexact-monotonic-milliseconds:
          divided-by 1000.0
        send:
          dc
          translate
          give
            send: canvas get-width
            divided-by 2
          give
            send: canvas get-height
            divided-by 2
        send:
          dc
          rotate
          give
            time
            plus 0.5
            times 7.5
            sin
            times 0.5
        send:
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
        send: dc translate -50 -50
        draw-pict:
          pict
          dc
          0
          0
        refresh:

refresh:
gives
  new:
    timer%
    interval interval
    just-once? true
    notify-callback function giving
      send: canvas refresh

send: frame show true
