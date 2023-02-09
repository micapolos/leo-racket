#lang leo

require:
  racket/class
  racket/gui/base
  pict

drawing-context
invoke-preserving-transformation function
does:
  define transformation send: drawing-context get-transformation
  invoke function
  send: drawing-context set-transformation transformation

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
    from: canvas drawing-context
    doing
      drawing-context
      invoke-preserving-transformation function doing
        define time
          the current-inexact-milliseconds
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
          send: drawing-context translate half-width half-height
          send: drawing-context rotate rotation
          send: drawing-context scale scale scale
          send: drawing-context translate -50 -50
          draw-pict: pict drawing-context 0 0

        send: frame is-shown?
        if true? the refresh
        else the void

the refresh
does new:
  timer%
  interval interval
  just-once? true
  notify-callback function doing send: canvas refresh

send: frame show true
