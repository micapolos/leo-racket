#lang leo/typed

(define-type Letter Char)
(define-type Word (Stackof Letter))

; -------------------------------------------------------------

(define letter-parser : (Parser Char Letter)
  (parser-filter
    char-parser
    (lambda (($char : Char))
      (and (char-alphabetic? $char) (char-lower-case? $char)))))

(check-equal?
  (parser-value-option (parser-plus letter-parser #\a))
  #\a)

(check-fail
  (parser-plus letter-parser #\A)
  "invalid value: #\\A")

(check-fail
  (parser-plus letter-parser #\1)
  "invalid value: #\\1")

; --------------------------------------------------------------

(define word-parser
  (parser-then
    letter-parser
    (lambda (($letter : Letter))
      (push-parser (stack $letter) letter-parser))))

(check-equal?
  (parser-string-option word-parser)
  #f)

(check-equal?
  (parser-string-option (parser-plus-string word-parser "foo"))
  "foo")

(check-fail
  (parser-plus-string word-parser "fo2")
  "invalid value: #\\2")
