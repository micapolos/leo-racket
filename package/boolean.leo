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
    boolean.negate
    does boolean.switch
      true boolean false
      false boolean true

  recipe
    boolean
    and boolean
    does boolean.switch
      true and.boolean
      false boolean false

  recipe
    boolean
    or boolean
    does boolean.switch
      true boolean true
      false or.boolean
