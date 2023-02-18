#lang racket

(require
  racket/class
  racket/gui/base
  leo/script/lang/base
  pict)

(define (invoke-preserving-transformation drawing-context function)
  (define transformation (send drawing-context get-transformation))
  (function)
  (send drawing-context set-transformation transformation))

(define interval 16)

(define pict (jack-o-lantern 100))

(define frame 
  (new
    frame%
    (label "Hello, world!")
    (width 640)
    (height 480)))

(define canvas 
  (new canvas%
    (parent frame)
    (paint-callback 
      (lambda (canvas drawing-context)
        (invoke-preserving-transformation drawing-context
          (lambda ()
            (define time
              (divided-by 
                (current-inexact-milliseconds) 
                1000.0))

            (define half-width
              (divided-by 
                (send canvas get-width) 
                2))

            (define half-height
              (divided-by
                (send canvas get-height) 
                2))

            (define rotation
              (times 
                (sin 
                  (times 
                    (plus time 0.5) 
                    7.5)) 
                0.5))

            (define scale
              (plus 
                (sin 
                  (times time 15)) 
                2.5))

            (send drawing-context translate half-width half-height)
            (send drawing-context rotate rotation)
            (send drawing-context scale scale scale)
            (send drawing-context translate -50 -50)
            (draw-pict pict drawing-context 0 0)
            (if (send frame is-shown?) (refresh) (void))))))))

(define (refresh)
  (new timer%
    (interval interval)
    (just-once? true)
    (notify-callback (lambda () (send canvas refresh)))))

(send frame show true)
