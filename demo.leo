#lang leo

select
  the "foo"
  not number
do switch
  the
    "text: "
    append text
  the
    "number: "
    append number.text
