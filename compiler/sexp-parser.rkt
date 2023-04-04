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
              (cond
                ((eqv? $char #\space)
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
  (check-equal? (parse-string $parser "\n") (failure! (invalid #\newline) (expected #\space) (at (position 1 1))))
  (check-equal? (parse-string $parser " ") (failure-at parse-incomplete (position 1 2)))
  (check-equal? (parse-string $parser " \n") (failure! (invalid #\newline) (expected #\space) (at (position 1 2))))
  (check-equal? (parse-string $parser "  ") "")
  (check-equal? (parse-string $parser "  \n") (failure-at parse-incomplete (position 2 1)))
  (check-equal? (parse-string $parser "  a") "a")
  (check-equal? (parse-string $parser "  ab") "ab")
  (check-equal? (parse-string $parser "  ab\n") (failure-at parse-incomplete (position 2 1)))
  (check-equal? (parse-string $parser "  ab\n  cd") "ab\ncd"))

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
  (parser-or
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
  (parser-bind (parser-or (parser `plus) sign-parser)
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
  (parser-or
    text-literal-parser
    integer-literal-parser))

; -----------------------------------------------------------------------------------------

(data (V I) env
  (literal-parser-fn : (-> V (Parser I)))
  (begin-parser-fn : (-> V Symbol (Parser I)))
  (combine-fn : (-> V I V)))

(: env-value-parser : (All (V I) (-> (Env V I) V (Parser V))))
(define (env-value-parser $env $value)
  (parser-or
    (parser $value)
    (parser-map (env-item-parser $env $value)
      (lambda (($item : I))
        (#%app (env-combine-fn $env) $value $item)))))

(: env-item-parser : (All (V I) (-> (Env V I) V (Parser I))))
(define (env-item-parser $env $value)
  (parser-or
    (env-literal-parser $env $value)
    (env-sentence-parser $env $value)))

(: env-literal-parser : (All (V I) (-> (Env V I) V (Parser I))))
(define (env-literal-parser $env $value)
  (#%app (env-literal-parser-fn $env) $value))

(: env-sentence-parser : (All (V I) (-> (Env V I) V (Parser I))))
(define (env-sentence-parser $env $value)
  (parser-bind word-parser
    (lambda (($word : Word))
      (env-rhs-parser $env $value (word-symbol $word)))))

(: env-begin-parser : (All (V I) (-> (Env V I) V Symbol (Parser I))))
(define (env-begin-parser $env $value $symbol)
  (#%app (env-begin-parser-fn $env) $value $symbol))

(: env-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser I))))
(define (env-rhs-parser $env $value $symbol)
  (parser-or
    (env-space-rhs-parser $env $value $symbol)
    (env-newline-rhs-parser $env $value $symbol)))

(: env-space-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser I))))
(define (env-space-rhs-parser $env $value $symbol)
  (prefix-parser
    space-parser
    (env-begin-parser $env $value $symbol)))

(: env-newline-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser I))))
(define (env-newline-rhs-parser $env $value $symbol)
  (prefix-parser
    newlines-parser
    (indented-parser
      (prefix-parser
        newlines-parser
        (env-begin-parser $env $value $symbol)))))

; -----------------------------------------------------------------------------------------

(define line-stack-env : (Env (Stackof Line) Line)
  (env
    (lambda ((_ : (Stackof Line)))
      literal-parser)
    (lambda (($line-stack : (Stackof Line)) ($symbol : Symbol))
      (parser-map (env-value-parser line-stack-env $line-stack)
        (lambda (($rhs-line-stack : (Stackof Line)))
          (sentence $symbol $rhs-line-stack))))
    (lambda (($line-stack : (Stackof Line)) ($line : Line))
      (push $line-stack $line))))

; -----------------------------------------------------------------------------------------

(data sentence
  (symbol : Symbol)
  (line-stack : (Stackof Line)))

(define (sentence-sexp ($sentence : Sentence)) : Sexp
  (define $symbol (sentence-symbol $sentence))
  (define $sexp-list (reverse (map line-sexp (sentence-line-stack $sentence))))
    (cond
      ((null? $sexp-list) $symbol)
      (else `(,$symbol ,@$sexp-list))))

(define-type Line (U Word Literal Sentence))

(define (line-sexp ($line : Line)) : Sexp
  (cond
    ((word? $line) (word-symbol $line))
    ((integer-literal? $line) (integer-literal-integer $line))
    ((text-literal? $line) (text-literal-string $line))
    ((sentence? $line) (sentence-sexp $line))))

(define (recursive-line-parser) : (Parser Line)
  (parser-or
    literal-parser
    sentence-parser))

(define sentence-parser : (Parser Sentence)
  (parser-bind word-parser
    (lambda (($word : Word))
      (parser-map rhs-line-stack-parser
        (lambda (($rhs-line-stack : (Stackof Line)))
          (sentence (word-symbol $word) $rhs-line-stack))))))

(define empty-rhs-line-stack-parser : (Parser (Stackof Line))
  (parser null))

(define space-rhs-line-stack-parser : (Parser (Stackof Line))
  (prefix-parser
    space-parser
    (singleton-stack-parser (recursive-line-parser))))

(define newline-rhs-line-stack-parser : (Parser (Stackof Line))
  (prefix-parser
    newlines-parser
    (indented-parser
      (separated-non-empty-stack-parser
        newlines-parser
        (recursive-line-parser)))))

(define rhs-line-stack-parser : (Parser (Stackof Line))
  (parser-or
    empty-rhs-line-stack-parser
    space-rhs-line-stack-parser
    newline-rhs-line-stack-parser))

(define line-parser : (Parser Line)
  (recursive-line-parser))

(define line-stack-parser : (Parser (Stackof Line))
  (stack-parser (parser-suffix line-parser newlines-parser)))

(define (parse-sexp ($string : String)) : (U Sexp (Failure Any))
  (parse-map line-parser $string line-sexp))

(define (parse-sexp-list ($string : String)) : (U (Listof Sexp) (Failure Any))
  (parse-map
    (prefix-parser maybe-newlines-parser line-stack-parser)
    $string
    (lambda (($line-stack : (Stackof Line)))
      (reverse (map line-sexp $line-stack)))))

(check-equal? (parse-sexp "\"one\"") "one")

(check-equal? (parse-sexp "one") `one)
(check-equal? (parse-sexp "one two") `(one two))
(check-equal? (parse-sexp "one two three") `(one (two three)))

(check-equal? (parse-sexp "one\n  two") `(one two))
(check-equal? (parse-sexp "one\n  two\n    three") `(one (two three)))
(check-equal? (parse-sexp "one\n\n  two") `(one two))

(check-equal? (parse-sexp "one\n  two three") `(one (two three)))
(check-equal? (parse-sexp "one two\n  three") `(one (two three)))

(check-equal? (parse-sexp "one\n  two\n  three") `(one two three))
(check-equal? (parse-sexp "one\n  two too\n  three free") `(one (two too) (three free)))

(check-equal? (parse-sexp "") (failure-at parse-incomplete (position 1 1)))
(check-equal? (parse-sexp "One") (failure-at parse-complete (position 1 2)))
(check-equal? (parse-sexp "bąk") (failure-at parse-complete (position 1 3)))
(check-equal? (parse-sexp "one-two") (failure-at parse-complete (position 1 5)))
(check-equal? (parse-sexp "123") 123)

(check-equal? (parse-sexp-list "") `())
(check-equal? (parse-sexp-list "one\n") `(one))
(check-equal? (parse-sexp-list "one\ntwo\n") `(one two))

(check-equal? (parse-sexp-list "one\n\ntwo\n\n") `(one two))

(check-equal? (parse-sexp-list "\n\none\n\ntwo\n\n") `(one two))
