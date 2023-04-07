#lang leo/typed

(define (char-letter? ($char : Char)) : Boolean
  (and
    (char>=? $char #\a)
    (char<=? $char #\z)))

(define (opaque-letter? ($any : Any)) : Boolean
  (and (char? $any) (char-letter? $any)))

(define-type Letter (Opaque opaque-letter?))

(define-predicate letter? Letter)

(define (letter-char ($letter : Letter)) : Char
  (cast $letter Char))

(define (char-letter-option ($char : Char)) : (Option Letter)
  (and (letter? $char) $char))

(define letter-parser : (Parser Letter)
  (parser-bind char-parser
    (lambda (($char : Char))
      (bind $letter-option (char-letter-option $char)
        (cond
          ($letter-option (parser $letter-option))
          (else (failure! (invalid $char))))))))

(check-equal? (parse letter-parser "") (failure-at parse-incomplete (position 1 1)))
(check-equal? (parse letter-parser "a") #\a)
(check-equal? (parse letter-parser "ą") (failure-at parse-incomplete (position 1 2))) ; TODO invalid-char
(check-equal? (parse letter-parser "A") (failure-at parse-incomplete (position 1 2))) ; TODO invalid-char
(check-equal? (parse letter-parser "1") (failure-at parse-incomplete (position 1 2))) ; TODO invalid-char
(check-equal? (parse letter-parser "ab") (failure-at parse-complete (position 1 2)))

; -------------------------------------------------------------------

(data word
  (non-empty-letter-stack : (Non-Empty-Stackof Letter)))

(define (word-symbol ($word : Word)) : Symbol
  (string->symbol
    (list->string
      (reverse
        (map letter-char
          (word-non-empty-letter-stack $word))))))

(define word-parser : (Parser Word)
  (parser-map
    (non-empty-stack-parser letter-parser)
    word))

(define (parse-word-symbol ($string : String)) : (U Symbol (Failure Any))
  (bind $result (parse word-parser $string)
    (cond
      ((word? $result) (word-symbol $result))
      ((failure? $result) $result))))

(check-equal? (parse-word-symbol "") (failure-at parse-incomplete (position 1 1)))
(check-equal? (parse-word-symbol "a") `a)
(check-equal? (parse-word-symbol "ab") `ab)
(check-equal? (parse-word-symbol "ab1") (failure-at parse-incomplete (position 1 4))) ; TODO: invalid-char
(check-equal? (parse-word-symbol "abA") (failure-at parse-incomplete (position 1 4))) ; TODO: invalid-char

; -----------------------------------------------------------------

(: done-remaining-indented-parser : (All (V) (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer (Parser V) (Parser V))))
(define (done-remaining-indented-parser $done $remaining $parser)
  (parser-bind-progress $parser
    (lambda (($progress : (Progress V)))
      (progress
        (and (= $remaining 0) (progress-value $progress))
        (lambda (($char : Char))
          (cond
            ((= $remaining 0)
              (bind $plus-parser (progress-plus-char $progress $char)
                (case $char
                  ((#\newline)
                    (done-remaining-indented-parser 0 $done $plus-parser))
                  (else
                    (done-remaining-indented-parser $done $remaining $plus-parser)))))
            (else
              (case $char
                ((#\newline)
                  (if (= $done 0)
                    (done-remaining-indented-parser $done $remaining (progress-plus-char $progress $char))
                    (failure! (invalid $char) (expected #\space))))
                ((#\space)
                  (done-remaining-indented-parser
                    (add1 $done)
                    (sub1 $remaining)
                    $parser))
                (else (failure! (invalid $char) (expected #\space)))))))))))

(: indented-parser : (All (V) (-> (Parser V) (Parser V))))
(define (indented-parser $parser)
  (done-remaining-indented-parser 0 2 $parser))

(bind $parser (indented-parser (stack-parser char-parser))
  (check-equal? (parse-string $parser "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse-string $parser "\n") (failure! parse-incomplete (at (position 2 1))))
  (check-equal? (parse-string $parser " ") (failure-at parse-incomplete (position 1 2)))
  (check-equal? (parse-string $parser " \n") (failure! (invalid #\newline) (expected #\space) (at (position 1 2))))
  (check-equal? (parse-string $parser "  ") "")
  (check-equal? (parse-string $parser "  \n") (failure-at parse-incomplete (position 2 1)))
  (check-equal? (parse-string $parser "  a") "a")
  (check-equal? (parse-string $parser "  ab") "ab")
  (check-equal? (parse-string $parser "  ab\n") (failure-at parse-incomplete (position 2 1)))
  (check-equal? (parse-string $parser "  ab\n  cd") "ab\ncd")
  (check-equal? (parse-string $parser "  ab\n\n  cd") "ab\n\ncd")
)

; -----------------------------------------------------------------------------------------

(data text-literal
  (char-stack : (Stackof Char)))

(define (text-literal-string ($text-literal : Text-Literal)) : String
  (list->string (reverse (text-literal-char-stack $text-literal))))

(define text-literal-parser : (Parser Text-Literal)
  (prefix-parser-suffix
   (exact-char-parser #\")
   (parser-map
     (stack-parser
       (parser-filter char-parser
         (lambda (($char : Char)) (not (eqv? $char #\")))))
     text-literal)
   (exact-char-parser #\")))

(define (parse-literal-string ($string : String)) : (U String (Failure Any))
  (bind $result (parse text-literal-parser $string)
    (cond
      ((text-literal? $result) (text-literal-string $result))
      ((failure? $result) $result))))

(check-equal? (parse-literal-string "") (failure-at parse-incomplete (position 1 1)))
(check-equal? (parse-literal-string "\"") (failure-at parse-incomplete (position 1 2)))
(check-equal? (parse-literal-string "\"\"") "")
(check-equal? (parse-literal-string "\"\"") "")
(check-equal? (parse-literal-string "\"abcABC123\n\"") "abcABC123\n")
(check-equal? (parse-literal-string "\"\"a") (failure-at parse-complete (position 1 3)))

; -----------------------------------------------------------------------------------------

(define-type Digit (U 0 1 2 3 4 5 6 7 8 9))

(define digit-parser : (Parser Digit)
  (parser-map char-parser
    (lambda (($char : Char))
      (bind $number (- (char->integer $char) (char->integer #\0))
        (and (>= $number 0) (<= $number 9) (cast $number Digit))))))

(check-equal? (parse digit-parser "") (failure-at parse-incomplete (position 1 1)))
(check-equal? (parse digit-parser "0") 0)
(check-equal? (parse digit-parser "9") 9)
(check-equal? (parse digit-parser "a") (failure-at parse-incomplete (position 1 2)))
(check-equal? (parse digit-parser "0a") (failure-at parse-complete (position 1 2)))
(check-equal? (parse digit-parser "a0") (failure-at parse-complete (position 1 2)))

; -----------------------------------------------------------------------------------------

(define-type Nonnegative-Integer-Literal (Non-Empty-Stackof Digit))

(define
  (nonnegative-integer-literal-integer
    ($nonnegative-integer-literal : Nonnegative-Integer-Literal))
  : Exact-Nonnegative-Integer
  (fold
    0
    (reverse $nonnegative-integer-literal)
    (lambda (($lhs : Exact-Nonnegative-Integer) ($rhs : Digit))
      (+ (* 10 $lhs) $rhs))))

(define nonnegative-integer-literal-parser : (Parser Nonnegative-Integer-Literal)
  (non-empty-stack-parser digit-parser))

(bind parse-integer
  (lambda (($string : String))
    (bind $result (parse nonnegative-integer-literal-parser $string)
      (cond
        ((stack? $result) (nonnegative-integer-literal-integer $result))
        ((failure? $result) $result))))
  (check-equal? (parse-integer "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse-integer "0") 0)
  (check-equal? (parse-integer "9") 9)
  (check-equal? (parse-integer "123") 123)
  (check-equal? (parse-integer "3.14") (failure-at parse-complete (position 1 3)))
  (check-equal? (parse-integer "123456789012345678901234567890123456789012345678901234567890")
    123456789012345678901234567890123456789012345678901234567890))

; ------------------------------------------------------------------------------------

(define-type Sign (U 'minus 'plus))

(define (sign-multiplier ($sign : Sign))
  (case $sign
    ((minus) -1)
    ((plus) 1)))

(define sign-parser : (Parser Sign)
  (first-parser
    (parser-map (exact-char-parser #\+) (lambda ((_ : True)) `plus))
    (parser-map (exact-char-parser #\-) (lambda ((_ : True)) `minus))))

(bind $parser sign-parser
  (check-equal? (parse $parser "+") `plus)
  (check-equal? (parse $parser "-") `minus)
  (check-equal? (parse $parser "*") (failure! (invalid #\*) (expected #\-) (at (position 1 1))))) ; TODO: wrong failure

; --------------------------------------------------------------------------------------

(data integer-literal
  (sign : Sign)
  (nonnegative-integer-literal : Nonnegative-Integer-Literal))

(define (integer-literal-integer ($integer-literal : Integer-Literal)) : Integer
  (*
    (sign-multiplier (integer-literal-sign $integer-literal))
    (nonnegative-integer-literal-integer
      (integer-literal-nonnegative-integer-literal $integer-literal))))

(define integer-literal-parser : (Parser Integer-Literal)
  (parser-bind (value-or-parser `plus sign-parser)
    (lambda (($sign : Sign))
      (parser-map nonnegative-integer-literal-parser
        (lambda (($nonnegative-integer-literal : Nonnegative-Integer-Literal))
          (integer-literal $sign $nonnegative-integer-literal))))))

(bind parse-integer
  (lambda (($string : String))
    (bind $result (parse integer-literal-parser $string)
      (cond
        ((integer-literal? $result) (integer-literal-integer $result))
        ((failure? $result) $result))))
  (check-equal? (parse-integer "") (failure-at parse-incomplete (position 1 1)))
  (check-equal? (parse-integer "123") 123)
  (check-equal? (parse-integer "+123") 123)
  (check-equal? (parse-integer "-123") -123)
  (check-equal? (parse-integer "*123") (failure-at parse-complete (position 1 2))))

; -----------------------------------------------------------------------------------------

(define-type Literal (U Text-Literal Integer-Literal))

(define literal-parser : (Parser Literal)
  (first-parser
    text-literal-parser
    integer-literal-parser))

(define (literal-sexp ($literal : Literal)) : Sexp
  (cond
    ((integer-literal? $literal) (integer-literal-integer $literal))
    ((text-literal? $literal) (text-literal-string $literal))))
