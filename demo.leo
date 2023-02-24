#lang leo

require:
  racket/unsafe/ops
  racket/math

any
  fixnum
  plus fixnum
  giving fixnum
does racket unsafe-fx+

fixnum 2
plus fixnum 3
check-equals fixnum 5

any
  fixnum
  minus fixnum
  giving fixnum
does racket unsafe-fx-

fixnum 5
minus fixnum 3
check-equals fixnum 2

any
  fixnum
  times fixnum
  giving fixnum
does racket unsafe-fx*

fixnum 2
times fixnum 3
check-equals fixnum 6

any
  flonum
  plus flonum
  giving flonum
does racket unsafe-fl+

flonum 2.25
plus flonum 3.5
check-equals flonum 5.75

any
  flonum
  minus flonum
  giving flonum
does racket unsafe-fl-

flonum 5.75
minus flonum 3.5
check-equals flonum 2.25

any
  flonum
  times flonum
  giving flonum
does racket unsafe-fl*

flonum 2.5
times flonum 1.25
check-equals flonum 3.125

any
  number
  equals number
  giving boolean
does racket: =

2
equals 2
check-equals true

2
equals 3
check-equals false

2
equals 2
then "equal"
else "not equal"
check-equals "equal"

2
equals 3
then "equal"
else "not equal"
check-equals "not equal"

any
  number
  plus number
  giving number
does racket +

2
plus 3
check-equals 5

any
  number
  minus number
  giving number
does racket -

5
minus 3
check-equals 2

any
  number
  times number
  giving number
does racket *

2
times 3
check-equals 6

any
  number
  less-than number
  giving boolean
does racket <

2
less-than 3
check-equals true

3
less-than 3
check-equals false

any
  number
  greater-than number
  giving boolean
does racket >

4
greater-than 3
check-equals true

4
greater-than 4
check-equals false

any
  number
  to-string
  giving string
does racket number->string

128
to-string
check-equals "128"

define pi
  racket pi
  as number

pi.number
greater-than 3.14
check-equals true

pi.number
less-than 3.15
check-equals true

define:
  zero 0
  one 1
  two
    one.number
    plus one.number

zero.number
check-equals 0

one.number
check-equals 1

two.number
check-equals 2

any number.squared
does
  number
  times number

16
squared
check-equals 256

any
  circle radius number
  area
does
  pi.number
  times circle.radius.number.squared

circle radius 10
area
check-equals
  pi.number
  times 100

any
  string
  append string
  giving string
does racket string-append

"Hello, "
append "world!"
check-equals "Hello, world!"

any
  string.length
  giving number
does racket string-length

"foo"
length
check-equals 3

any
  string.exclamate
  giving string
does
  string
  append "!!!"

"Hello"
exclamate
check-equals "Hello!!!"

comment: local binding
"Hello, "
append "world"
exclamate
do
  string
  append " ("
  append string.length.to-string
  append ")"
check-equals "Hello, world!!! (15)"

comment: local bindings
the: 10 "apples"
do
  number.to-string
  append " "
  append string
check-equals "10 apples"

comment: function with one argument
number
doing
  number
  plus 1
apply 10
check-equals 11

comment: function with two arguments
the: number string
doing
  number.to-string
  append " "
  append string
apply: 10 "apples"
check-equals "10 apples"

any
  number
  fibonacci
  giving number
does recursively
  number
  less-than 2
  then number
  else
    number
    minus 2
    fibonacci
    plus
      number
      minus 1
      fibonacci

10
fibonacci
check-equals 55
