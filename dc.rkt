#lang leo

require:
  racket/class
  racket/gui/base

dc
invoke-preserving-transformation fn
does:
  variable transformation send: dc get-transformation
  invoke fn
  send: dc set-transformation transformation
