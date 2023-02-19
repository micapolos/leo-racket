#lang typed/racket/base

(require 
  leo/typed/base
  leo/testing)

(data string-parser (string : String) (length : Index))

(define 
  (string-parser-complete-string ($string-parser : StringPrefix))
  : (Option String)
  (let (($string (string-parser-string $string-parser))
        ($length (string-parser-length $string-parser)))
  (and
    (>= $length (string-length $string))
    $string)))

(check-equal?
  (string-parser-complete-string (string-parser "ab" 1))
  #f)

(check-equal?
  (string-parser-complete-string (string-parser "ab" 2))
  "ab")

(check-equal?
  (string-parser-complete-string (string-parser "ab" 3))
  "ab")

(define 
  (string-parser+char 
    ($string-parser : StringPrefix) 
    ($char : Char))
  : (Option StringPrefix)
  (let (($string (string-parser-string $string-parser))
        ($length (string-parser-length $string-parser)))
    (and 
      (< $length (string-length $string))
      (equal? (string-ref $string $length) $char)
      (string-parser $string (cast (+ $length 1) Index)))))

(check-equal?
  (string-parser+char (string-parser "ab" 0) #\a)
  (string-parser "ab" 1))

(check-equal?
  (string-parser+char (string-parser "ab" 0) #\b)
  #f)

(check-equal?
  (string-parser+char (string-parser "ab" 1) #\b)
  (string-parser "ab" 2))

(check-equal?
  (string-parser+char (string-parser "ab" 1) #\a)
  #f)

(check-equal?
  (string-parser+char (string-parser "ab" 2) #\a)
  #f)
