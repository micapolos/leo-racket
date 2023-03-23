#lang leo

recipe
  boolean true
  does boolean choice
    the true
    not false

recipe
  boolean false
  does boolean choice
    not true
    the false
