#lang leo/typed

(define (char-letter? ($char : Char))
  (and
    (char-alphabetic? $char)
    (char-lower-case? $char)))

; -------------------------------------------------------------------

(define letter-parser
  (parser-filter char-parser char-letter?))

(check-equal? (parse letter-parser "") #f)
(check-equal? (parse letter-parser "a") #\a)
(check-equal? (parse letter-parser "A") #f)
(check-equal? (parse letter-parser "1") #f)
(check-equal? (parse letter-parser "ab") #f)

; -------------------------------------------------------------------

(define lazy-symbol-parser : (Parser (Lazy Symbol))
  (parser-bind letter-parser
    (lambda (($letter : Char))
      (parser-map
        (push-parser (stack $letter) letter-parser)
        (lambda (($char-stack : (Stackof Char)))
          (lazy
            (string->symbol (list->string (reverse $char-stack)))))))))

(define symbol-parser
  (parser-map lazy-symbol-parser
    (lambda (($lazy-symbol : (Lazy Symbol)))
      (force $lazy-symbol))))

(check-equal? (parse symbol-parser "") #f)
(check-equal? (parse symbol-parser "a") `a)
(check-equal? (parse symbol-parser "ab") `ab)
(check-equal? (parse symbol-parser "ab1") #f)
(check-equal? (parse symbol-parser "abA") #f)

; -----------------------------------------------------------------

(: done-remaining-indented-parser : (All (V) (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer (Parser V) (Parser V))))
(define (done-remaining-indented-parser $done $remaining $parser)
  (and $parser
    (progress
      (and (= $remaining 0) (progress-value $parser))
      (lambda (($char : Char))
        (cond
          ((= $remaining 0)
            (bind $plus-parser (parser-plus-char $parser $char)
              (case $char
                ((#\newline)
                  (done-remaining-indented-parser 0 $done $plus-parser))
                (else
                  (done-remaining-indented-parser $done $remaining $plus-parser)))))
          (else
            (and
              (eqv? $char #\space)
              (done-remaining-indented-parser
                (add1 $done)
                (sub1 $remaining)
                $parser))))))))

(: indented-parser : (All (V) (-> (Parser V) (Parser V))))
(define (indented-parser $parser)
  (done-remaining-indented-parser 0 2 $parser))

(bind $parser (indented-parser (stack-parser char-parser))
  (check-equal? (parse-string $parser "") #f)
  (check-equal? (parse-string $parser "\n") #f)
  (check-equal? (parse-string $parser " ") #f)
  (check-equal? (parse-string $parser " \n") #f)
  (check-equal? (parse-string $parser "  ") "")
  (check-equal? (parse-string $parser "  \n") #f)
  (check-equal? (parse-string $parser "  a") "a")
  (check-equal? (parse-string $parser "  ab") "ab")
  (check-equal? (parse-string $parser "  ab\n") #f)
  (check-equal? (parse-string $parser "  ab\n  cd") "ab\ncd"))

; -----------------------------------------------------------------------------------------

(define text-literal-parser
  (parser-bind (exact-char-parser #\")
    (lambda ((_ : True))
      (parser-bind
        (stack-parser
          (parser-filter char-parser
            (lambda (($char : Char)) (not (eqv? $char #\")))))
        (lambda (($char-stack : (Stackof Char)))
          (parser-bind (exact-char-parser #\")
            (lambda ((_ : True))
              (parser (list->string (reverse $char-stack))))))))))

(check-equal? (parse text-literal-parser "") #f)
(check-equal? (parse text-literal-parser "\"") #f)
(check-equal? (parse text-literal-parser "\"\"") "")
(check-equal? (parse text-literal-parser "\"\"") "")
(check-equal? (parse text-literal-parser "\"abcABC123\n\"") "abcABC123\n")
(check-equal? (parse text-literal-parser "\"\"a") #f)

; -----------------------------------------------------------------------------------------

(define digit-parser : (Parser Exact-Nonnegative-Integer)
  (parser-map char-parser
    (lambda (($char : Char))
      (bind $number (- (char->integer $char) (char->integer #\0))
        (and (>= $number 0) (<= $number 9) $number)))))

(check-equal? (parse digit-parser "") #f)
(check-equal? (parse digit-parser "0") 0)
(check-equal? (parse digit-parser "9") 9)
(check-equal? (parse digit-parser "a") #f)
(check-equal? (parse digit-parser "0a") #f)
(check-equal? (parse digit-parser "a0") #f)

(define lazy-exact-nonnegative-integer-parser : (Parser (Lazy Exact-Nonnegative-Integer))
  (parser-bind digit-parser
    (lambda (($digit : Exact-Nonnegative-Integer))
      (parser-map (push-parser (stack $digit) digit-parser)
        (lambda (($digit-stack : (Stackof Exact-Nonnegative-Integer)))
          (lazy
            (fold
              0
              (reverse $digit-stack)
              (lambda (($lhs : Exact-Nonnegative-Integer) ($rhs : Exact-Nonnegative-Integer))
                (+ (* 10 $lhs) $rhs)))))))))

(bind $parser lazy-exact-nonnegative-integer-parser
  (check-equal? (option-app #%app (parse $parser "")) #f)
  (check-equal? (option-app #%app (parse $parser "0")) 0)
  (check-equal? (option-app #%app (parse $parser "9")) 9)
  (check-equal? (option-app #%app (parse $parser "123")) 123)
  (check-equal? (option-app #%app (parse $parser "3.14")) #f)
  (check-equal?
    (option-app #%app (parse $parser "123456789012345678901234567890123456789012345678901234567890"))
    123456789012345678901234567890123456789012345678901234567890))

(define sign-multiplier-parser : (Parser Integer)
  (parser-or
    (parser 1)
    (parser-or
      (parser-map (exact-char-parser #\+) (lambda ((_ : True)) 1))
      (parser-map (exact-char-parser #\-) (lambda ((_ : True)) -1)))))

(bind $parser sign-multiplier-parser
  (check-equal? (parse $parser "") 1)
  (check-equal? (parse $parser "+") 1)
  (check-equal? (parse $parser "-") -1)
  (check-equal? (parse $parser "*") #f))

(define lazy-integer-parser : (Parser (Lazy Integer))
  (parser-bind sign-multiplier-parser
    (lambda (($sign : Integer))
      (parser-map lazy-exact-nonnegative-integer-parser
        (lambda (($lazy-exact-nonnegative-integer : (Lazy Exact-Nonnegative-Integer)))
          (lazy
            (* $sign ($lazy-exact-nonnegative-integer))))))))

(bind $parser lazy-integer-parser
  (check-equal? (option-app #%app (parse $parser "")) #f)
  (check-equal? (option-app #%app (parse $parser "123")) 123)
  (check-equal? (option-app #%app (parse $parser "+123")) 123)
  (check-equal? (option-app #%app (parse $parser "-123")) -123)
  (check-equal? (option-app #%app (parse $parser "*123")) #f))

; -----------------------------------------------------------------------------------------

(define lazy-literal-parser : (Parser (Lazy Sexp))
  (parser-or
    (parser-map text-literal-parser (lambda (($text : String)) (lazy $text)))
    lazy-integer-parser))

; -----------------------------------------------------------------------------------------

(define lazy-sexp-parser : (Parser (Lazy Sexp))
  (parser-or
    lazy-literal-parser
    (parser-bind lazy-symbol-parser
      (lambda (($lazy-symbol : (Lazy Symbol)))
        (parser-or
          (parser $lazy-symbol)
          (parser-or
            (parser-bind (exact-char-parser #\space)
              (lambda ((_ : True))
                (parser-map lazy-sexp-parser
                  (lambda (($lazy-sexp : (Lazy Sexp)))
                    (lazy
                      `(
                        ,(force $lazy-symbol)
                        ,(force $lazy-sexp)))))))
            (parser-bind (exact-char-parser #\newline)
              (lambda ((_ : True))
                (parser-map
                  (indented-parser
                    (separated-non-empty-stack-parser
                      (exact-char-parser #\newline)
                      lazy-sexp-parser))
                  (lambda (($non-empty-lazy-sexp-stack : (Non-Empty-Stackof (Lazy Sexp))))
                    (lazy
                      `(
                        ,(force $lazy-symbol)
                        ,@(reverse
                          (map
                            (lambda (($lazy-sexp : (Lazy Sexp))) (force $lazy-sexp))
                            $non-empty-lazy-sexp-stack))))))))))))))

(define sexp-line-parser : (Parser Sexp)
  (parser-bind lazy-sexp-parser
    (lambda (($lazy-sexp : (Lazy Sexp)))
      (parser-map (exact-char-parser #\newline)
        (lambda ((_ : True)) (force $lazy-sexp))))))

(define sexp-stack-parser : (Parser (Stackof Sexp))
  (stack-parser sexp-line-parser))

(define (parse-sexp ($string : String)) : (Option Sexp)
  (option-app #%app (parse lazy-sexp-parser $string)))

(define (parse-sexp-list ($string : String)) : (Option (Listof Sexp))
  (option-app reverse (parse sexp-stack-parser $string)))

(check-equal? (parse-sexp "\"one\"") "one")

(check-equal? (parse-sexp "one") `one)
(check-equal? (parse-sexp "one two") `(one two))
(check-equal? (parse-sexp "one two three") `(one (two three)))

(check-equal? (parse-sexp "one\n  two") `(one two))
(check-equal? (parse-sexp "one\n  two\n    three") `(one (two three)))

(check-equal? (parse-sexp "one\n  two three") `(one (two three)))
(check-equal? (parse-sexp "one two\n  three") `(one (two three)))

(check-equal? (parse-sexp "one\n  two\n  three") `(one two three))
(check-equal? (parse-sexp "one\n  two too\n  three free") `(one (two too) (three free)))

(check-equal? (parse-sexp "") #f)
(check-equal? (parse-sexp "One") #f)
(check-equal? (parse-sexp "one-two") #f)
(check-equal? (parse-sexp "123") 123)

(check-equal? (parse-sexp-list "") `())
(check-equal? (parse-sexp-list "one\n") `(one))
(check-equal? (parse-sexp-list "one\ntwo\n") `(one two))
