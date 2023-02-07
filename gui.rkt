#lang leo

do require:
  racket/class
  racket/gui/base
  pict

do define: interval 16

do define:
  dt
  divided-by: 1 60

do define: time 0.0

new:
  frame%
  label "Hello, world!"
  width 640
  height 640
as frame in
  function
    for time
    giving
      jack-o-lantern 100
      translate: -50 -50
      scale
        time
        times 15
        sin
        plus 3
  as make-pict in
    do new:
      timer%
      interval interval
      notify-callback function giving:
        send: frame refresh
        set!:
          time
          plus: time dt
    new:
      canvas%
      parent frame
      paint-callback function
        for: canvas dc
        giving draw-pict:
          give
            make-pict
            invoke time
          dc
          give
            send: canvas get-width
            divided-by 2
          give
            send: canvas get-height
            divided-by 2
  give: frame
send: show true
