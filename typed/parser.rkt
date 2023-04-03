#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/stack
  leo/typed/option
  leo/typed/failure
  leo/typed/testing
  (for-syntax
    racket/base
    (only-in leo/typed/base fold)))

(define-type (Parser V)
  (U
    (Failure Any) ; TODO: Make it generic
    (Progress V)))

(data (V) progress
  (value : (Option V))
  (plus-fn : (-> Char (Parser V))))

; -----------------------------------------------------------------------------------------

(define parse-complete
  `(parse complete))

(define parse-incomplete
  `(parse incomplete))

(define (invalid-char ($char : Char))
  `(invalid ,$char))

(define (invalid-value ($value : Any))
  `(invalid ,$value))

(define (invalid-expected-char ($invalid : Char) ($expected : Char))
  `(error (invalid ,$invalid) (expected ,$expected)))

(define parse-complete-failure
  (failure parse-complete))

(define parse-incomplete-failure
  (failure parse-incomplete))

(define (invalid-char-failure ($char : Char)) : (Failure Any)
  (failure (invalid-char $char)))

(define (invalid-expected-char-failure ($invalid : Char) ($expected : Char)) : (Failure Any)
  (failure (invalid-expected-char $invalid $expected)))

(define #:forall (V) (invalid-value-failure ($value : V)) : (Failure Any)
  (failure (invalid-value $value)))

(define (failure-at ($value : Any) ($position : Position)) : (Failure Any)
  (failure `(error ,$value (at ,$position))))

; -----------------------------------------------------------------------------------------

(data position
  (row : Exact-Positive-Integer)
  (column : Exact-Positive-Integer))

(define start-position (position 1 1))

(define (position-newline ($position : Position)) : Position
  (position
    (add1 (position-row $position))
    1))

(define (position-next-column ($position : Position)) : Position
  (position
    (position-row $position)
    (add1 (position-column $position))))

(define (position-plus-char ($position : Position) ($char : Char)) : Position
  (case $char
    ((#\newline) (position-newline $position))
    (else (position-next-column $position))))

; -----------------------------------------------------------------------------------------

(data (V) positioned
  (value : V)
  (position : Position))

; -----------------------------------------------------------------------------------------

(define #:forall (V) (progress-value-or-failure ($progress : (Progress V))) : (U V (Failure Any))
  (or
    (progress-value $progress)
    parse-incomplete-failure))

(define #:forall (V) (parser ($value : (Option V))) : (Parser V)
  (progress $value
    (lambda (($char : Char))
      parse-complete-failure)))

(define #:forall (V) (parser-plus-char ($parser : (Parser V)) ($char : Char)) : (Parser V)
  (cond
    ((progress? $parser) (#%app (progress-plus-fn $parser) $char))
    ((failure? $parser) $parser)))

(define #:forall (V)
  (positioned-parser-plus-string
    ($initial-positioned-parser : (Positioned (Parser V)))
    ($string : String))
  : (Positioned (Parser V))
  (fold
    $initial-positioned-parser
    (string->list $string)
    (lambda (($positioned-parser : (Positioned (Parser V))) ($char : Char))
      (define $parser (positioned-value $positioned-parser))
      (define $position (positioned-position $positioned-parser))
      (cond
        ((progress? $parser)
          (positioned
            (bind $plus-parser (parser-plus-char $parser $char)
              (cond
                ((progress? $plus-parser) $plus-parser)
                ((failure? $plus-parser)
                  (failure-at
                    (failure-value $plus-parser)
                    $position))))
            (position-plus-char $position $char)))
        ((failure? $parser)
          $positioned-parser)))))

(define #:forall (V) (parse ($start-parser : (Parser V)) ($string : String)) : (U V (Failure Any))
  (bind $positioned-parser (positioned-parser-plus-string (positioned $start-parser start-position) $string)
    (define $parser (positioned-value $positioned-parser))
    (define $position (positioned-position $positioned-parser))
    (cond
      ((progress? $parser)
        (option-or
          (progress-value $parser)
          (failure-at parse-incomplete $position)))
      ((failure? $parser)
        $parser))))

(define #:forall (V R) (parse-map ($parser : (Parser V)) ($string : String) ($fn : (-> V R))) : (U R (Failure Any))
  (bind $result (parse $parser $string)
    (cond
      ((failure? $result) $result)
      (else ($fn $result)))))

(define (parse-string ($char-stack-parser : (Parser (Stackof Char))) ($string : String)) : (U String (Failure Any))
  (parse-map $char-stack-parser $string
    (lambda (($char-stack : (Stackof Char)))
      (list->string (reverse $char-stack)))))

; -----------------------------------------------------------------------------------------

(define char-parser : (Parser Char)
  (progress #f
    (lambda (($char : Char))
      (parser $char))))

(check-equal?
  (parse char-parser "")
  (failure-at parse-incomplete (position 1 1)))

(check-equal?
  (parse char-parser "a")
  #\a)

(check-equal?
  (parse char-parser "ab")
  (failure-at parse-complete (position 1 2)))

; -----------------------------------------------------------------------------------------

(define (char-filter-parser ($fn : (-> Char Boolean))) : (Parser Char)
  (progress #f
    (lambda (($char : Char))
      (cond
        (($fn $char) (parser $char))
        (else (invalid-char-failure $char))))))

(bind $parser (char-filter-parser char-numeric?)
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser "1") #\1)
  (check-equal? (parse $parser "a") (failure-at (invalid-char #\a) (position 1 1)))
  (check-equal? (parse $parser "12") (failure-at parse-complete (position 1 2))))

; -----------------------------------------------------------------------------------------

(define numeric-char-parser (char-filter-parser char-numeric?))
(define alphabetic-char-parser (char-filter-parser char-alphabetic?))

; -----------------------------------------------------------------------------------------

(define (exact-char-parser ($char : Char)) : (Parser True)
  (progress #f
    (lambda (($next-char : Char))
      (cond
        ((equal? $char $next-char) (parser #t))
        (else (invalid-expected-char-failure $next-char $char))))))

(bind $parser (exact-char-parser #\a)
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser "a") #t)
  (check-equal? (parse $parser "b") (failure-at (invalid-expected-char #\b #\a) (position 1 1)))
  (check-equal? (parse $parser "ab") (failure-at parse-complete (position 1 2))))

(define dot-char-parser
  (progress #f
    (lambda (($first-char : Char))
      (cond
        ((char=? $first-char #\.)
          (progress #f
            (lambda (($second-char : Char))
              (parser $second-char))))
        (else
          (invalid-expected-char-failure $first-char #\.))))))

(define (dot-last-char-parser ($last-char : (Option Char))) : (Parser Char)
  (progress $last-char
    (lambda (($first-char : Char))
      (cond
        ((equal? $first-char #\.)
          (progress #f
            (lambda (($second-char : Char))
              (dot-last-char-parser $second-char))))
        (else
          (invalid-char-failure $first-char))))))

(bind $parser (dot-last-char-parser #f)
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser ".") (failure-at parse-incomplete (position 1 2)))
  (check-equal? (parse $parser ".a") #\a)
  (check-equal? (parse $parser ".a.") (failure-at parse-incomplete (position 1 4)))
  (check-equal? (parse $parser ".a.b") #\b)
  (check-equal? (parse $parser ".a:") (failure-at (invalid-char #\:) (position 1 3)))
  (check-equal? (parse $parser ".a:b") (failure-at (invalid-char #\:) (position 1 3)))
  (check-equal? (parse $parser ":a") (failure-at (invalid-char #\:) (position 1 1)))
  (check-equal? (parse $parser ":a.") (failure-at (invalid-char #\:) (position 1 1)))
  (check-equal? (parse $parser ":a.b") (failure-at (invalid-char #\:) (position 1 1))))

; -----------------------------------------------------------------------------------------

(define space-parser (exact-char-parser #\space))
(define newline-parser (exact-char-parser #\newline))

; -----------------------------------------------------------------------------------------

(define (exact-char-list-parser ($char-list : (Listof Char))) : (Parser True)
  (cond
    ((null? $char-list)
      (parser #t))
    (else
      (progress #f
        (lambda (($char : Char))
          (cond
            ((char=? $char (car $char-list))
              (exact-char-list-parser (cdr $char-list)))
            (else
              (invalid-expected-char-failure
                $char
                (car $char-list)))))))))

(define (exact-string-parser ($string : String)) : (Parser True)
  (exact-char-list-parser (string->list $string)))

(bind $parser (exact-string-parser "")
  (check-equal? (parse $parser "") #t)
  (check-equal? (parse $parser "f") (failure-at parse-complete (position 1 1))))

(bind $parser (exact-string-parser "fo")
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser "f") (failure-at parse-incomplete (position 1 2)))
  (check-equal? (parse $parser "fo") #t)
  (check-equal? (parse $parser "fof") (failure-at parse-complete (position 1 3))))

; -----------------------------------------------------------------------------------------

(: parser-or-bind : (All (I O) (-> (Parser I) (Option (Parser O)) (-> I (Parser O)) (Parser O))))
(define (parser-or-bind $left-parser $right-parser-option $fn)
  (cond
    ((progress? $left-parser)
      (bind $left-value (progress-value $left-parser)
        (cond
          ($left-value
            (bind $right-parser ($fn $left-value)
              (cond
                ((progress? $right-parser)
                  (progress
                    (progress-value $right-parser)
                    (lambda (($char : Char))
                      (parser-or-bind
                        (parser-plus-char $left-parser $char)
                        (parser-plus-char $right-parser $char)
                        $fn))))
                ((failure? $right-parser)
                  (progress #f
                    (lambda (($char : Char))
                      (parser-or-bind
                        (parser-plus-char $left-parser $char)
                        #f
                        $fn)))))))
          (else
            (or
              (option-bind $right-parser-option $right-parser
                (cond
                  ((progress? $right-parser)
                    (progress
                      (progress-value $right-parser)
                      (lambda (($char : Char))
                          (parser-or-bind
                            (parser-plus-char $left-parser $char)
                            (parser-plus-char $right-parser $char)
                            $fn))))
                  ((failure? $right-parser)
                    (progress #f
                      (lambda (($char : Char))
                        (parser-or-bind
                          (parser-plus-char $left-parser $char)
                          #f
                          $fn))))))
              (progress #f
                (lambda (($char : Char))
                  (parser-or-bind
                    (parser-plus-char $left-parser $char)
                    #f
                    $fn))))))))
    ((failure? $left-parser)
      (or $right-parser-option $left-parser))))

(: parser-bind : (All (I O) (-> (Parser I) (-> I (Parser O)) (Parser O))))
(define (parser-bind $parser $fn)
  (parser-or-bind $parser parse-complete-failure $fn))

(bind $parser
  (parser-bind (dot-last-char-parser #f)
    (lambda (($char : Char))
      (parser-bind (exact-char-parser #\.)
        (lambda ((_ : True))
          (parser (string $char #\.))))))
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser ".") (failure-at parse-incomplete (position 1 2)))
  (check-equal? (parse $parser ".a") (failure-at parse-incomplete (position 1 3)))
  (check-equal? (parse $parser ".a.") "a.")
  (check-equal? (parse $parser ".a.b") (failure-at parse-incomplete (position 1 5)))
  (check-equal? (parse $parser ".a.b.") "b."))

(let
  (($parser (parser-bind parse-complete-failure (lambda (($char : Char)) parse-complete-failure))))
  (check-equal? (parse $parser "") parse-complete-failure)
  (check-equal? (parse $parser "a") parse-complete-failure))

(let
  (($parser (parser-bind char-parser (lambda (($char : Char)) (parser (string $char))))))
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser "a") "a")
  (check-equal? (parse $parser "ab") (failure-at parse-complete (position 1 2))))

(let
  (($parser
    (parser-bind char-parser
      (lambda (($left-char : Char))
        (parser-bind char-parser
          (lambda (($right-char : Char))
            (parser (string $left-char $right-char))))))))
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser "a") (failure-at parse-incomplete (position 1 2)))
  (check-equal? (parse $parser "ab") "ab")
  (check-equal? (parse $parser "abc") (failure-at parse-complete (position 1 3))))

; ---------------------------------------------------------------------------------------

(: parser-map : (All (I O) (-> (Parser I) (-> I O) (Parser O))))
(define (parser-map $parser $fn)
  (parser-bind $parser
    (lambda (($value : I))
      (parser ($fn $value)))))

(: parser-filter : (All (V) (-> (Parser V) (-> V Boolean) (Parser V))))
(define (parser-filter $parser $fn)
  (cond
    ((progress? $parser)
      (bind $value (progress-value $parser)
        (cond
          ($value
            (cond
              (($fn $value)
                (progress $value
                  (lambda (($char : Char))
                    (parser-filter (#%app (progress-plus-fn $parser) $char) $fn))))
              (else
                (invalid-value-failure $value))))
          (else
            (progress $value
              (lambda (($char : Char))
                (parser-filter (#%app (progress-plus-fn $parser) $char) $fn)))))))
    ((failure? $parser) $parser)))

(bind $parser (parser-filter char-parser char-alphabetic?)
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser "a") #\a)
  (check-equal? (parse $parser "1") (failure-at (invalid-value #\1) (position 1 1)))
  (check-equal? (parse $parser "ab") (failure-at parse-complete (position 1 2))))

; ---------------------------------------------------------------------------------

(define #:forall (V) (prefix-parser ($prefix : (Parser True)) ($parser : (Parser V))) : (Parser V)
  (parser-bind $prefix
    (lambda ((_ : True))
      (parser-bind $parser
        (lambda (($value : V))
          (parser $value))))))

(define #:forall (V) (parser-suffix ($parser : (Parser V)) ($suffix : (Parser True))) : (Parser V)
  (parser-bind $parser
    (lambda (($value : V))
      (parser-bind $suffix
        (lambda ((_ : True))
          (parser $value))))))

(define #:forall (V) (prefix-parser-suffix ($prefix : (Parser True)) ($parser : (Parser V)) ($suffix : (Parser True))) : (Parser V)
  (parser-bind $prefix
    (lambda ((_ : True))
      (parser-bind $parser
        (lambda (($value : V))
          (parser-bind $suffix
            (lambda ((_ : True))
              (parser $value))))))))

; ---------------------------------------------------------------------------------

(: parser-or2 : (All (V) (-> (Parser V) (Parser V) (Parser V))))
(define (parser-or2 $left-parser $right-parser)
  (cond
    ((progress? $left-parser)
      (cond
        ((progress? $right-parser)
          (progress
            (or
              (progress-value $left-parser)
              (progress-value $right-parser))
            (lambda (($char : Char))
              (parser-or
                (parser-plus-char $left-parser $char)
                (parser-plus-char $right-parser $char)))))
        ((failure $right-parser) $left-parser)))
    ((failure? $left-parser) $right-parser)))

(define-syntax (parser-or $syntax)
  (syntax-case $syntax ()
    ((_ first rest ...)
      (fold
        #`first
        (syntax-e #`(rest ...))
        (lambda ($lhs $rhs)
          #`(parser-or2 #,$lhs #,$rhs))))))

(bind $parser (parser-or (parser "default") (exact-char-parser #\a))
  (check-equal? (parse $parser "") "default")
  (check-equal? (parse $parser "a") #t)
  (check-equal? (parse $parser "b") (failure-at (invalid-expected-char #\b #\a) (position 1 1))))

(bind $parser (parser-or (parser-filter char-parser char-alphabetic?) (parser-filter char-parser char-numeric?))
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser "a") #\a)
  (check-equal? (parse $parser "1") #\1)
  (check-equal? (parse $parser " ") (failure-at (invalid-value #\space) (position 1 1))))

; -------------------------------------------------------------------------------

(: push-parser : (All (V) (-> (Stackof V) (Parser V) (Parser (Stackof V)))))
(define (push-parser $stack $parser)
  (parser-or
    (parser $stack)
    (parser-bind $parser
      (lambda (($item : V))
        (push-parser (push $stack $item) $parser)))))

(: stack-parser : (All (V) (-> (Parser V) (Parser (Stackof V)))))
(define (stack-parser $parser)
  (push-parser null $parser))

(define #:forall (V) (singleton-stack-parser ($parser : (Parser V))) : (Parser (Stackof V))
  (parser-map $parser
    (lambda (($value : V))
      (stack $value))))

(bind $parser (stack-parser dot-char-parser)
  (check-equal? (parse $parser "") (stack))

  (check-equal? (parse $parser ".") (failure-at parse-incomplete (position 1 2)))
  (check-equal? (parse $parser ".a") (stack #\a))
  (check-equal? (parse $parser ":a") (failure-at (invalid-expected-char #\: #\.) (position 1 1)))

  (check-equal? (parse $parser ".a.") (failure-at parse-incomplete (position 1 4)))
  (check-equal? (parse $parser ".a.b") (stack #\a #\b))
  (check-equal? (parse $parser ".a:b") (failure-at (invalid-expected-char #\: #\.) (position 1 3))))

; -----------------------------------------------------------------------------

(: non-empty-stack-parser : (All (V) (-> (Parser V) (Parser (Non-Empty-Stackof V)))))
(define (non-empty-stack-parser $item-parser)
  (parser-bind $item-parser
    (lambda (($item : V))
      (parser-map
        (push-parser (stack $item) $item-parser)
        (lambda (($item-stack : (Stackof V)))
          (ann
            (pair (top $item-stack) (pop $item-stack))
            (Non-Empty-Stackof V)))))))

(bind $parser (non-empty-stack-parser dot-char-parser)
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser ".a") (non-empty-stack #\a))
  (check-equal? (parse $parser ".a.b") (non-empty-stack #\a #\b))
  (check-equal? (parse $parser ".a.b.c") (non-empty-stack #\a #\b #\c)))

; -----------------------------------------------------------------------------

(: separated-non-empty-stack-parser : (All (V) (-> (Parser True) (Parser V) (Parser (Non-Empty-Stackof V)))))
(define (separated-non-empty-stack-parser $separator-parser $item-parser)
  (parser-bind $item-parser
    (lambda (($item : V))
      (parser-map
        (push-parser (stack $item)
          (prefix-parser $separator-parser $item-parser))
        (lambda (($item-stack : (Stackof V)))
          (ann
            (pair (top $item-stack) (pop $item-stack))
            (Non-Empty-Stackof V)))))))

(bind $parser (separated-non-empty-stack-parser (exact-string-parser ", ") dot-char-parser)
  (check-equal? (parse $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse $parser ".a") (non-empty-stack #\a))
  (check-equal? (parse $parser ".a, .b") (non-empty-stack #\a #\b))
  (check-equal? (parse $parser ".a, .b, .c") (non-empty-stack #\a #\b #\c)))

; ------------------------------------------------------------------------------

(define (zero-or-more-parser ($parser : (Parser True)))
  (parser-map (stack-parser $parser)
    (lambda ((_ : (Stackof True))) #t)))

(define (one-or-more-parser ($parser : (Parser True)))
  (parser-map (non-empty-stack-parser $parser)
    (lambda ((_ : (Non-Empty-Stackof True))) #t)))

(define newlines-parser (one-or-more-parser newline-parser))
(define maybe-newlines-parser (zero-or-more-parser newline-parser))
