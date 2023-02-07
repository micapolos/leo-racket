#lang leo

do require:
  racket/class
  racket/gui/base

do
  dc
  draw-preserving-transformation fn
  gives:
    do define:
      transformation
      send: dc get-transformation
    do invoke fn
    do send: dc set-transformation transformation
