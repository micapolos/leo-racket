#lang leo

require:
  racket/class
  racket/draw
  racket/gui/base

do:
  define bitmap make-bitmap: 128 128
  define drawing new:
    bitmap-dc%
    bitmap bitmap
  send: drawing set-pen "green" 3 'solid
  send: drawing set-brush "blue" 'solid
  send: drawing draw-ellipse 16 16 96 96
  make-object: image-snip% bitmap
