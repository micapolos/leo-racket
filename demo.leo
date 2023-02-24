#lang leo

require:
  racket/unsafe/ops
  racket/math

any duplicate thing
does result:
  original thing
  duplicated thing

duplicate 123
check-equal? result:
  original 123
  duplicated 123

any
  fixnum
  plus fixnum
  giving fixnum
does racket unsafe-fx+

fixnum 2
plus fixnum 3
check-equal? fixnum 5

any
  fixnum
  minus fixnum
  giving fixnum
does racket unsafe-fx-

fixnum 5
minus fixnum 3
check-equal? fixnum 2

any
  fixnum
  times fixnum
  giving fixnum
does racket unsafe-fx*

fixnum 2
times fixnum 3
check-equal? fixnum 6

any
  flonum
  plus flonum
  giving flonum
does racket unsafe-fl+

flonum 2.25
plus flonum 3.5
check-equal? flonum 5.75

any
  flonum
  minus flonum
  giving flonum
does racket unsafe-fl-

flonum 5.75
minus flonum 3.5
check-equal? flonum 2.25

any
  flonum
  times flonum
  giving flonum
does racket unsafe-fl*

flonum 2.5
times flonum 1.25
check-equal? flonum 3.125

any
  number
  equals number
  giving boolean
does racket: =

2
equals 2
check-equal? true

2
equals 3
check-equal? false

2
equals 2
then "equal"
else "not equal"
check-equal? "equal"

2
equals 3
then "equal"
else "not equal"
check-equal? "not equal"

any
  number
  plus number
  giving number
does racket +

2
plus 3
check-equal? 5

any
  number
  minus number
  giving number
does racket -

5
minus 3
check-equal? 2

any
  number
  times number
  giving number
does racket *

2
times 3
check-equal? 6

any
  number
  less-than? number
  giving boolean
does racket <

2
less-than? 3
check-equal? true

3
less-than? 3
check-equal? false

any
  number
  greater-than? number
  giving boolean
does racket >

4
greater-than? 3
check-equal? true

4
greater-than? 4
check-equal? false

any
  number
  to-string
  giving string
does racket number->string

128
to-string
check-equal? "128"

make pi
  racket pi
  as number

pi.number
greater-than? 3.14
check-equal? true

pi.number
less-than? 3.15
check-equal? true

make zero 0

zero.number
check-equal? 0

make one 1

one.number
check-equal? 1

make two
  one.number
  plus one.number

two.number
check-equal? 2

any number.squared
does
  number
  times number

16
squared
check-equal? 256

any
  circle radius number
  area
does
  pi.number
  times circle.radius.number.squared

circle radius 10
area
check-equal?
  pi.number
  times 100

any
  string
  append string
  giving string
does racket string-append

"Hello, "
append "world!"
check-equal? "Hello, world!"

any
  string.length
  giving number
does racket string-length

"foo"
length
check-equal? 3

any
  string.exclamate
  giving string
does
  string
  append "!!!"

"Hello"
exclamate
check-equal? "Hello!!!"

comment: local binding
"Hello, "
append "world"
exclamate
do
  string
  append " ("
  append string.length.to-string
  append ")"
check-equal? "Hello, world!!! (15)"

comment: local bindings
the: 10 "apples"
do
  number.to-string
  append " "
  append string
check-equal? "10 apples"

comment: function without arguments
doing
  2
  plus 2
apply
check-equal? 4

comment: function with one argument
number
doing
  number
  plus 1
apply 10
check-equal? 11

comment: function with two arguments
the: number string
doing
  number.to-string
  append " "
  append string
apply: 10 "apples"
check-equal? "10 apples"

comment: top level expression
circle:
  center point:
    x 10
    y 20
  radius 2
  area
    pi.number
    times squared 2
