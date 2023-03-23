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

then
  recipe
    not boolean
    does not.boolean.switch
      boolean false
      boolean true

  recipe
    boolean
    and boolean
    does boolean.switch
      the and.boolean
      the boolean false

  recipe
    boolean
    or boolean
    does boolean.switch
      the boolean true
      the or.boolean
