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
  string
  giving string
does racket number->string

128
string
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
