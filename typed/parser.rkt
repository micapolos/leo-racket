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

(define-type (Parser V) (Option (Progress V)))

(data (V) progress
  (value : (Option V))
  (plus-fn : (-> Char (Parser V))))

; -----------------------------------------------------------------------------------------

(define #:forall (V) (parser ($value : (Option V))) : (Parser V)
  (progress $value (lambda (($char : Char)) #f)))

(define #:forall (V) (parser-plus-char ($parser : (Parser V)) ($char : Char)) : (Parser V)
  (and $parser ((progress-plus-fn $parser) $char)))

(define #:forall (V) (parser-plus-string ($parser : (Parser V)) ($string : String)) : (Parser V)
  (fold
    $parser
    (string->list $string)
    (lambda (($parser : (Parser V)) ($char : Char))
      (and $parser (parser-plus-char $parser $char)))))

(define #:forall (V) (parse ($parser : (Parser V)) ($string : String)) : (Option V)
  (bind $parser (parser-plus-string $parser $string)
    (and $parser (progress-value $parser))))

(define (parse-string ($char-stack-parser : (Parser (Stackof Char))) ($string : String)) : (Option String)
  (option-bind (parser-plus-string $char-stack-parser $string) $progress
    (option-bind (progress-value $progress) $char-stack
      (list->string (reverse $char-stack)))))

; -----------------------------------------------------------------------------------------

(define char-parser : (Parser Char)
  (progress #f
    (lambda (($char : Char))
      (parser $char))))

(check-equal?
  (parse char-parser "")
  #f)

(check-equal?
  (parse char-parser "a")
  #\a)

(check-equal?
  (parse char-parser "ab")
  #f)

; -----------------------------------------------------------------------------------------

(define (char-filter-parser ($fn : (-> Char Boolean))) : (Parser Char)
  (progress #f
    (lambda (($char : Char))
      (and ($fn $char) (parser $char)))))

(bind $parser (char-filter-parser char-numeric?)
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser "1") #\1)
  (check-equal? (parse $parser "a") #f)
  (check-equal? (parse $parser "12") #f))

; -----------------------------------------------------------------------------------------

(define numeric-char-parser (char-filter-parser char-numeric?))
(define alphabetic-char-parser (char-filter-parser char-alphabetic?))

; -----------------------------------------------------------------------------------------

(define (exact-char-parser ($char : Char)) : (Parser True)
  (progress #f
    (lambda (($next-char : Char))
      (parser (equal? $char $next-char)))))

(bind $parser (exact-char-parser #\a)
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser "a") #t)
  (check-equal? (parse $parser "b") #f)
  (check-equal? (parse $parser "ab") #f))

(define dot-char-parser
  (progress #f
    (lambda (($first-char : Char))
      (and (equal? $first-char #\.)
        (progress #f
          (lambda (($second-char : Char))
            (parser $second-char)))))))

(define (dot-last-char-parser ($last-char : (Option Char))) : (Parser Char)
  (progress $last-char
    (lambda (($first-char : Char))
      (and (equal? $first-char #\.)
        (progress #f
          (lambda (($second-char : Char))
            (dot-last-char-parser $second-char)))))))

(bind $parser (dot-last-char-parser #f)
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser ".") #f)
  (check-equal? (parse $parser ".a") #\a)
  (check-equal? (parse $parser ".a.") #f)
  (check-equal? (parse $parser ".a.b") #\b)
  (check-equal? (parse $parser ".a:") #f)
  (check-equal? (parse $parser ".a:b") #f)
  (check-equal? (parse $parser ":a") #f)
  (check-equal? (parse $parser ":a.") #f)
  (check-equal? (parse $parser ":a.b") #f))

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
          (and
            (eqv? $char (car $char-list))
            (exact-char-list-parser (cdr $char-list))))))))

(define (exact-string-parser ($string : String)) : (Parser True)
  (exact-char-list-parser (string->list $string)))

(bind $parser (exact-string-parser "")
  (check-equal? (parse $parser "") #t)
  (check-equal? (parse $parser "f") #f))

(bind $parser (exact-string-parser "fo")
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser "f") #f)
  (check-equal? (parse $parser "fo") #t)
  (check-equal? (parse $parser "fof") #f))

; -----------------------------------------------------------------------------------------

(: parser-or-bind : (All (I O) (-> (Parser I) (Parser O) (-> I (Parser O)) (Parser O))))
(define (parser-or-bind $left-parser $right-parser $fn)
  (cond
    ($left-parser
      (bind $left-value (progress-value $left-parser)
        (cond
          ($left-value
            (bind $new-right-parser ($fn $left-value)
              (cond
                ($new-right-parser
                  (progress
                    (progress-value $new-right-parser)
                    (lambda (($char : Char))
                      (parser-or-bind
                        (parser-plus-char $left-parser $char)
                        (parser-plus-char $new-right-parser $char)
                        $fn))))
                (else
                  (progress #f
                    (lambda (($char : Char))
                      (parser-or-bind
                        (parser-plus-char $left-parser $char)
                        (parser-plus-char $right-parser $char)
                        $fn)))))))
          (else
            (cond
              ($right-parser
                (progress
                  (progress-value $right-parser)
                  (lambda (($char : Char))
                      (parser-or-bind
                        (parser-plus-char $left-parser $char)
                        (parser-plus-char $right-parser $char)
                        $fn))))
              (else
                (progress #f
                  (lambda (($char : Char))
                    (parser-or-bind
                      (parser-plus-char $left-parser $char)
                      (parser-plus-char $right-parser $char)
                      $fn)))))))))
    (else $right-parser)))

(: parser-bind : (All (I O) (-> (Parser I) (-> I (Parser O)) (Parser O))))
(define (parser-bind $parser $fn)
  (parser-or-bind $parser #f $fn))

(bind $parser
  (parser-bind (dot-last-char-parser #f)
    (lambda (($char : Char))
      (parser-bind (exact-char-parser #\.)
        (lambda ((_ : True))
          (parser (string $char #\.))))))
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser ".") #f)
  (check-equal? (parse $parser ".a") #f)
  (check-equal? (parse $parser ".a.") "a.")
  (check-equal? (parse $parser ".a.b") #f)
  (check-equal? (parse $parser ".a.b.") "b."))

(let
  (($parser (parser-bind #f (lambda (($char : Char)) #f))))
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser "a") #f))

(let
  (($parser (parser-bind char-parser (lambda (($char : Char)) (parser (string $char))))))
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser "a") "a")
  (check-equal? (parse $parser "ab") #f))

(let
  (($parser
    (parser-bind char-parser
      (lambda (($left-char : Char))
        (parser-bind char-parser
          (lambda (($right-char : Char))
            (parser (string $left-char $right-char))))))))
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser "a") #f)
  (check-equal? (parse $parser "ab") "ab")
  (check-equal? (parse $parser "abc") #f))

; ---------------------------------------------------------------------------------------

(: parser-map : (All (I O) (-> (Parser I) (-> I O) (Parser O))))
(define (parser-map $parser $fn)
  (parser-bind $parser
    (lambda (($value : I))
      (parser ($fn $value)))))

(: parser-filter : (All (V) (-> (Parser V) (-> V Boolean) (Parser V))))
(define (parser-filter $parser $fn)
  (parser-bind $parser
    (lambda (($value : V))
      (and ($fn $value) (parser $value)))))

(check-equal?
  (parse (parser-filter char-parser char-alphabetic?) "")
  #f)

(check-equal?
  (parse (parser-filter char-parser char-alphabetic?) "a")
  #\a)

(check-equal?
  (parse (parser-filter char-parser char-alphabetic?) "1")
  #f)

(check-equal?
  (parse (parser-filter char-parser char-alphabetic?) "ab")
  #f)

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
(define (parser-or2 $parser1 $parser2)
  (cond
    ($parser1
      (cond
        ($parser2
          (progress
            (or
              (progress-value $parser1)
              (progress-value $parser2))
            (lambda (($char : Char))
              (parser-or
                (parser-plus-char $parser1 $char)
                (parser-plus-char $parser2 $char)))))
        (else $parser1)))
    (else $parser2)))

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
  (check-equal? (parse $parser "b") #f))

(bind $parser (parser-or (parser-filter char-parser char-alphabetic?) (parser-filter char-parser char-numeric?))
  (check-equal? (parse $parser "") #f)
  (check-equal? (parse $parser "a") #\a)
  (check-equal? (parse $parser "1") #\1)
  (check-equal? (parse $parser " ") #f))

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

(bind $parser (stack-parser dot-char-parser)
  (check-equal? (parse $parser "") (stack))

  (check-equal? (parse $parser ".") #f)
  (check-equal? (parse $parser ".a") (stack #\a))
  (check-equal? (parse $parser ":a") #f)

  (check-equal? (parse $parser ".a.") #f)
  (check-equal? (parse $parser ".a.b") (stack #\a #\b))
  (check-equal? (parse $parser ".a:b") #f))

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
  (check-equal? (parse $parser "") #f)
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
  (check-equal? (parse $parser "") #f)
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
