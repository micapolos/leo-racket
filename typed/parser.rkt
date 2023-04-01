#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/failure
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  (for-syntax racket/base))

(data (I O) parser
  (value-option : (Option O))
  (plus-fn : (-> I (Parser I O))))

(define #:forall (I O) (parser-value ($parser : (Parser I O))) : O
  (or
    (parser-value-option $parser)
    (error "parser no value")))

(define #:forall (I) (parser-string-option ($parser : (Parser I (Stackof Char)))) : (Option String)
  (option-bind (parser-value-option $parser) $char-stack
    (list->string (reverse $char-stack))))

; ----------------------------------------------------------------------------------------

(define #:forall (I O) (value-parser ($value : O)) : (Parser I O)
  (parser $value
    (lambda (($item : I))
      (error "already parsed"))))

(check-equal?
  (parser-value (value-parser "foo"))
  "foo")

(check-fail
  (parser-value (value-parser #f))
  "parser no value")

; -----------------------------------------------------------------------------------------

(define #:forall (I O) (parser-plus ($parser : (Parser I O)) ($item : I)) : (Parser I O)
  ((parser-plus-fn $parser) $item))

(check-fail
  (parser-plus (value-parser "foo") #\a)
  "already parsed")

; ------------------------------------------------------------------------------------------

(define #:forall (I O) (parser-plus-list ($parser : (Parser I O)) ($item-list : (Listof I))) : (Parser I O)
  (fold
    $parser
    $item-list
    (lambda (($parser : (Parser I O)) ($item : I))
      (parser-plus $parser $item))))

(define #:forall (O) (parser-plus-string ($parser : (Parser Char O)) ($string : String)) : (Parser Char O)
  (parser-plus-list $parser (string->list $string)))

; -----------------------------------------------------------------------------------------

(define #:forall (I) (item-parser) : (Parser I I)
  (parser #f (lambda (($item : I)) (value-parser $item))))

(check-equal?
  (parser-value-option (parser-plus (item-parser) "foo"))
  "foo")

(check-fail
  (parser-plus (parser-plus (item-parser) "foo") "bar")
  "already parsed")

(define char-parser (ann (item-parser) (Parser Char Char)))

; ------------------------------------------------------------------------------------

(: parser-or (All (I O1 O2) (-> (Parser I O1) (Parser I O2) (Parser I (U O1 O2)))))
(define (parser-or $lhs-parser $rhs-parser)
  (parser
    (or
      (parser-value-option $lhs-parser)
      (parser-value-option $rhs-parser))
    (lambda (($item : I))
      (bind $lhs-result (or-failure (parser-plus $lhs-parser $item))
        (cond
          ((failure? $lhs-result) (parser-plus $rhs-parser $item))
          (else (parser-or $lhs-result (parser-plus $rhs-parser $item))))))))

(check-equal?
  (parser-value-option
    (parser-or
      (value-parser "foo")
      (value-parser 128)))
  "foo")

(check-equal?
  (parser-value-option
    (parser-or
      (value-parser #f)
      (value-parser 128)))
  128)

(check-equal?
  (parser-value-option
    (parser-or
      (value-parser "foo")
      (value-parser #f)))
  "foo")

(check-equal?
  (parser-value-option
    (parser-or
      (value-parser #f)
      (value-parser #f)))
  #f)

; -------------------------------------------------------------------------------------

(: parser-then (All (I O1 O2) (-> (Parser I O1) (-> O1 (Parser I O2)) (Parser I O2))))
(define (parser-then $parser $fn)
  (bind $value-option (parser-value-option $parser)
    (cond
      ($value-option
        (bind $then-parser ($fn $value-option)
          (parser
            (parser-value-option $then-parser)
            (lambda (($item : I))
              (with-handlers
                ((exn:fail?
                  (lambda (($exn : exn:fail))
                    (parser-plus $then-parser $item))))
                (parser-then (parser-plus $parser $item) $fn))))))
      (else
        (parser #f
          (lambda (($item : I)) : (Parser I O2)
            (parser-then (parser-plus $parser $item) $fn)))))))

(check-equal?
  (parser-value-option
    (parser-then
      (value-parser "foo")
      (lambda (($string : String))
        (value-parser (string-append $string "bar")))))
  "foobar")

(check-equal?
  (parser-value-option
    (parser-then
      char-parser
      (lambda (($char : Char)) (value-parser $char))))
  #f)

(check-equal?
  (parser-value-option
    (parser-plus-string
      (parser-then
        char-parser
        (lambda (($char : Char))
          (value-parser $char)))
      "a"))
  #\a)

(check-equal?
  (parser-value-option
    (parser-plus
      (parser-then
        (ann (item-parser) (Parser String String))
        (lambda (($string-1 : String))
          (parser-then
            (ann (item-parser) (Parser String String))
            (lambda (($string-2 : String))
              (value-parser (string-append $string-1 $string-2))))))
      "foo"))
  #f)

(check-equal?
  (parser-value-option
    (parser-plus-list
      (parser-then
        (ann (item-parser) (Parser String String))
        (lambda (($string-1 : String))
          (parser-then
            (ann (item-parser) (Parser String String))
            (lambda (($string-2 : String))
              (value-parser (string-append $string-1 $string-2))))))
      (list "foo" "bar")))
  "foobar")

; -----------------------------------------------------------------------------------

(: parser-map (All (I O1 O2) (-> (Parser I O1) (-> O1 O2) (Parser I O2))))
(define (parser-map $parser $fn)
  (parser-then $parser
    (lambda (($value : O1))
      (value-parser ($fn $value)))))

(define #:forall (I O) (parser-filter ($parser : (Parser I O)) ($fn : (-> O Boolean))) : (Parser I O)
  (parser-map $parser
    (lambda (($value : O))
      (cond
        (($fn $value) $value)
        (else (error (format "invalid value: ~s" $value)))))))

(: push-parser (All (I O) (-> (Stackof O) (Parser I O) (Parser I (Stackof O)))))
(define (push-parser $value-stack $item-parser)
  (parser-or
    (ann (value-parser $value-stack) (Parser I (Stackof O)))
    (parser-then
      $item-parser
      (lambda (($item : O))
        (push-parser
          (push $value-stack $item)
          $item-parser)))))

(define #:forall (I O) (stack-parser ($item-parser : (Parser I O))) : (Parser I (Stackof O))
  (push-parser null $item-parser))

(check-equal?
  (parser-string-option (stack-parser char-parser))
  "")

(check-equal?
  (parser-string-option
    (parser-plus-string
      (stack-parser char-parser)
      "a"))
  "a")

(check-equal?
  (parser-string-option
    (parser-plus-string
      (stack-parser char-parser)
      "ab"))
  "ab")
