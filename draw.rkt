#lang leo

do require:
  racket/class
  racket/draw
  racket/gui/base

do
  bitmap
  gives make-bitmap: 128 128

do
  drawing
  gives
    bitmap-dc%
    new bitmap bitmap

do send: drawing set-pen "green" 3 'solid
do send: drawing set-brush "blue" 'solid
do send: drawing draw-ellipse 16 16 96 96

do make-object: image-snip% bitmap
