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

(define symbol-fn-parser
  (parser-bind letter-parser
    (lambda (($letter : Char))
      (parser-map
        (push-parser (stack $letter) letter-parser)
        (lambda (($char-stack : (Stackof Char)))
          (lambda ()
            (string->symbol (list->string (reverse $char-stack)))))))))

(define symbol-parser
  (parser-map symbol-fn-parser
    (lambda (($symbol-fn : (-> Symbol)))
      ($symbol-fn))))

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
      (and (= $done 0) (progress-value $parser))
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
  (check-equal? (parse-string $parser "") "")
  (check-equal? (parse-string $parser "\n") #f)
  (check-equal? (parse-string $parser " ") #f)
  (check-equal? (parse-string $parser " \n") #f)
  (check-equal? (parse-string $parser "  ") #f)
  (check-equal? (parse-string $parser "  \n") "\n")
  (check-equal? (parse-string $parser "  a") #f)
  (check-equal? (parse-string $parser "  ab") #f)
  (check-equal? (parse-string $parser "  ab\n") "ab\n")
  (check-equal? (parse-string $parser "  ab\n  cd\n") "ab\ncd\n"))

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

(define digit-parser
  (parser-map numeric-char-parser
    (lambda (($char : Char))
      (- (char->integer $char) (char->integer #\0)))))

(check-equal? (parse digit-parser "") #f)
(check-equal? (parse digit-parser "0") 0)
(check-equal? (parse digit-parser "9") 9)
(check-equal? (parse digit-parser "a") #f)
(check-equal? (parse digit-parser "0a") #f)
(check-equal? (parse digit-parser "a0") #f)

; -----------------------------------------------------------------------------------------

(define literal-parser
  (parser-or
    text-literal-parser
    digit-parser))

; -----------------------------------------------------------------------------------------

(define atom-parser
  (parser-or symbol-parser text-literal-parser))

; -----------------------------------------------------------------------------------------

(define sexp-parser : (Parser Sexp)
  (parser-or
    atom-parser
    (parser-bind symbol-parser
      (lambda (($symbol : Symbol))
        (parser-or
          (parser $symbol)
          (parser-or
            (parser-bind (exact-char-parser #\space)
              (lambda ((_ : True))
                (parser-map sexp-parser
                  (lambda (($sexp : Sexp))
                    `(,$symbol ,$sexp)))))
            (parser-bind (exact-char-parser #\newline)
              (lambda ((_ : True))
                (parser-map (indented-parser sexp-stack-parser)
                  (lambda (($sexp-stack : (Stackof Sexp)))
                    (cond
                      ((null? $sexp-stack) $symbol)
                      (else `(,$symbol ,@(reverse $sexp-stack))))))))))))))

(define sexp-stack-parser : (Parser (Stackof Sexp))
  (stack-parser sexp-parser))

(define (parse-sexp ($string : String)) : (Option Sexp)
  (parse sexp-parser $string))

(define (parse-sexp-list ($string : String)) : (Option (Listof Sexp))
  (option-app reverse (parse sexp-stack-parser $string)))

(check-equal? (parse-sexp "\"one\"") "one")

(check-equal? (parse-sexp "one") `one)
(check-equal? (parse-sexp "one two") `(one two))
(check-equal? (parse-sexp "one two three") `(one (two three)))

(check-equal? (parse-sexp "one\n") `one)
(check-equal? (parse-sexp "one\n  two\n") `(one two))
(check-equal? (parse-sexp "one\n  two\n    three\n") `(one (two three)))

(check-equal? (parse-sexp "one\n  two three\n") `(one (two three)))
(check-equal? (parse-sexp "one two\n  three\n") `(one (two three)))

(check-equal? (parse-sexp "one\n  two\n  three\n") `(one two three))
(check-equal? (parse-sexp "one\n  two too\n  three free\n") `(one (two too) (three free)))

(check-equal? (parse-sexp "") #f)
(check-equal? (parse-sexp "One") #f)
(check-equal? (parse-sexp "one-two") #f)
(check-equal? (parse-sexp "123") #f)

(check-equal? (parse-sexp-list "") `())
(check-equal? (parse-sexp-list "one") `(one))
(check-equal? (parse-sexp-list "one\n") `(one))
(check-equal? (parse-sexp-list "one\ntwo") `(one two))
(check-equal? (parse-sexp-list "one\ntwo\n") `(one two))
