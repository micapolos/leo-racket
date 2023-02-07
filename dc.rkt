#lang leo

do require:
  racket/class
  racket/gui/base

do
  dc
  invoke-preserving-transformation fn
  gives:
    do
      transformation
      gives send: dc get-transformation
    do invoke fn
    do send: dc set-transformation transformation
