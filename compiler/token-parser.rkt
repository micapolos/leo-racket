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

(define symbol-fn-parser
  (parser-then
    letter-parser
    (lambda (($letter : Letter))
      (parser-map
        (push-parser (stack $letter) letter-parser)
        (lambda (($letter-stack : (Stackof Letter)))
          (lambda ()
            (string->symbol
              (list->string
                (reverse $letter-stack)))))))))

(check-equal?
  (parser-value-option symbol-fn-parser)
  #f)

(check-equal?
  (option-app #%app (parser-value-option (parser-plus-string symbol-fn-parser "foo")))
  `foo)

(check-fail
  (parser-plus-string symbol-fn-parser "fo2")
  "invalid value: #\\2")
