#lang leo

do require:
  slideshow/base
  slideshow/text

do slide:
  give: #:aspect `widescreen
  big big big t "Hello"
  big t "This is my first slideshow"
  item: "Remember to subscribe."
  item: "Don't forget to like me."
  item:
    "What"
    bt "else"
    "could I ask you for?"

do slide:
  give: #:aspect `widescreen
  big big big t "Now..."
  big t "What do you want me to do?"
