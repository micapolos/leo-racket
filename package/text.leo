#lang leo

racket
  "equal?"
  of recipe
    text
    equals text
    doing boolean

racket
  "equal?"
  of recipe
    text
    check equals text
    doing boolean

racket
  "string-append"
  of recipe
    text
    plus text
    doing text

racket
  "string-append"
  of recipe
    text
    append text
    doing text

racket
  "string-length"
  of recipe
    text
    get number of characters
    doing number
