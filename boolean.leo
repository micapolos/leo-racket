#lang leo

recipe
  boolean true
  does boolean select
    the true
    not false

recipe
  boolean false
  does boolean select
    not true
    the false
