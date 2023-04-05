#lang leo/typed

(require
  leo/compiler/sexp-parser)

(data (V I) env
  (begin-parser-fn :
    (->
      V ; currently parsed value
      Symbol ; begin symbol
      (->
        I ; starting inner value
        (-> I (Parser I)) ; inner item parser
        (-> I V) ; end function, accumulating inner value into outer value
        (Parser V))
      (Parser V))))

(: env-begin-parser : (All (V I) (-> (Env V I) V Symbol (-> I (-> I (Parser I)) (-> I V) (Parser V)) (Parser V))))
(define (env-begin-parser $env $value $symbol $item-parser-fn)
  (#%app (env-begin-parser-fn $env) $value $symbol $item-parser-fn))

; --------------------------------------------------------------------------------------

(define comma-separator-parser
  (exact-string-parser ", "))

(define newline-separator-parser
  newline-parser)

(define comma-or-newline-separator-parser
  (parser-or comma-separator-parser newline-separator-parser))

(: env-script-parser : (All (V I) (-> (Env V I) V (Parser V))))
(define (env-script-parser $env $value)
  (parser-or
    (parser $value)
    (parser-suffix
      (repeat-separated-parser $value comma-or-newline-separator-parser
        (lambda (($repeated-value : V))
          (env-sentence-parser $env $repeated-value)))
      newline-parser)))

(: env-sentence-parser : (All (V I) (-> (Env V I) V (Parser V))))
(define (env-sentence-parser $env $value)
  (parser-bind word-parser
    (lambda (($word : Word))
      (env-rhs-parser $env $value
        (word-symbol $word)))))

(: env-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-rhs-parser $env $value $symbol)
  (parser-or
    (env-symbol-parser $env $value $symbol)
    (env-dot-rhs-parser $env $value $symbol)
    (env-space-rhs-parser $env $value $symbol)
    (env-colon-rhs-parser $env $value $symbol)
    (env-parens-rhs-parser $env $value $symbol)
    (env-newline-rhs-parser $env $value $symbol)))

(: env-symbol-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-symbol-parser $env $value $symbol)
  (env-begin-parser $env $value $symbol
    (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
      (parser (#%app $end-fn $item)))))

(: env-dot-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-dot-rhs-parser $env $value $symbol)
  (prefix-parser (exact-char-parser #\.)
    (env-begin-parser $env $value $symbol
      (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
        (env-sentence-parser $env (#%app $end-fn $item))))))

(: env-space-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-space-rhs-parser $env $value $symbol)
  (prefix-parser space-parser
    (env-begin-parser $env $value $symbol
      (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
        (parser-map
          (#%app $item-parser-fn $item)
          (lambda (($parsed-item : I))
            (#%app $end-fn $parsed-item)))))))

(: env-colon-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-colon-rhs-parser $env $value $symbol)
  (prefix-parser (exact-string-parser ": ")
    (env-begin-parser $env $value $symbol
      (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
        (parser-map
          (repeat-separated-parser $item comma-separator-parser
            (lambda (($following-item : I))
              (#%app $item-parser-fn $following-item)))
          (lambda (($parsed-item : I))
            (#%app $end-fn $parsed-item)))))))

(: env-parens-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-parens-rhs-parser $env $value $symbol)
  (prefix-parser-suffix
    (exact-string-parser "(")
    (env-begin-parser $env $value $symbol
      (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
        (parser-or
          (parser (#%app $end-fn $item))
          (parser-map
            (repeat-separated-parser $item comma-separator-parser
              (lambda (($following-item : I))
                (#%app $item-parser-fn $following-item)))
            (lambda (($parsed-item : I))
              (#%app $end-fn $parsed-item))))))
    (exact-string-parser ")")))

(: env-newline-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-newline-rhs-parser $env $value $symbol)
  (prefix-parser newline-parser
    (indented-parser
      (env-begin-parser $env $value $symbol
        (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
          (parser-map
            (repeat-separated-parser $item comma-or-newline-separator-parser
              (lambda (($following-item : I))
                (#%app $item-parser-fn $following-item)))
            (lambda (($parsed-item : I))
              (#%app $end-fn $parsed-item))))))))

(let ()
  (define test-env : (Env (Stackof String) String)
    (env
      (lambda
        (
          ($string-stack : (Stackof String))
          ($symbol : Symbol)
          ($fn : (->
            String
            (-> String (Parser String))
            (-> String (Stackof String))
            (Parser (Stackof String)))))
        : (Parser (Stackof String))
        (#%app $fn
          (string-append
            (number->string (length $string-stack))
            "-"
            (symbol->string $symbol))
          (lambda (($string : String)) : (Parser String)
            (parser-map (non-empty-stack-parser numeric-char-parser)
              (lambda (($char-stack : (Stackof Char)))
                (string-append $string "+"
                  (list->string (reverse $char-stack))))))
          (lambda (($string : String)) : (Stackof String)
            (push $string-stack $string))))))

  (define sentence-parser : (Parser (Stackof String))
    (env-sentence-parser test-env (stack "foo" "bar")))

  (define script-parser : (Parser (Stackof String))
    (env-script-parser test-env null))

  (check-equal? (parse sentence-parser "foo") (stack "foo" "bar" "2-foo"))
  (check-equal? (parse sentence-parser "foo 123") (stack "foo" "bar" "2-foo+123"))

  (check-equal? (parse sentence-parser "foo\n  123") (stack "foo" "bar" "2-foo+123"))
  (check-equal? (parse sentence-parser "foo\n  123\n  456") (stack "foo" "bar" "2-foo+123+456"))

  (check-equal? (parse sentence-parser "foo: 123") (stack "foo" "bar" "2-foo+123"))
  (check-equal? (parse sentence-parser "foo: 123, 456") (stack "foo" "bar" "2-foo+123+456"))

  (check-equal? (parse sentence-parser "foo()") (stack "foo" "bar" "2-foo"))
  (check-equal? (parse sentence-parser "foo(123)") (stack "foo" "bar" "2-foo+123"))
  (check-equal? (parse sentence-parser "foo(123, 456)") (stack "foo" "bar" "2-foo+123+456"))

  (check-equal? (parse sentence-parser "goo.gar") (stack "foo" "bar" "2-goo" "3-gar"))
  (check-equal? (parse sentence-parser "goo.gar 123") (stack "foo" "bar" "2-goo" "3-gar+123"))
  (check-equal? (parse sentence-parser "goo.gar: 123") (stack "foo" "bar" "2-goo" "3-gar+123"))
  (check-equal? (parse sentence-parser "goo.gar: 123, 456") (stack "foo" "bar" "2-goo" "3-gar+123+456"))
  (check-equal? (parse sentence-parser "goo.gar\n  123\n  456") (stack "foo" "bar" "2-goo" "3-gar+123+456"))

  (check-equal? (parse sentence-parser "foo:")  (failure! parse-incomplete (at (position 1 5))))
  (check-equal? (parse sentence-parser "foo: ")  (failure! parse-incomplete (at (position 1 6))))
  (check-equal? (parse sentence-parser "foo(")  (failure! parse-incomplete (at (position 1 5))))
  (check-equal? (parse sentence-parser "foo.")  (failure! parse-incomplete (at (position 1 5))))
  (check-equal? (parse sentence-parser "foo,") (failure! parse-incomplete (at (position 1 5))))
  (check-equal? (parse sentence-parser "foo, ") (failure! parse-complete (at (position 1 5))))

  (check-equal? (parse sentence-parser "Foo") (failure! parse-complete (at (position 1 2))))
  (check-equal? (parse sentence-parser "fOo") (failure! parse-complete (at (position 1 3))))
  (check-equal? (parse sentence-parser "foO") (failure! parse-incomplete (at (position 1 4))))
  (check-equal? (parse sentence-parser "1") (failure! parse-incomplete (at (position 1 2))))
  (check-equal? (parse sentence-parser "fo1") (failure! parse-incomplete (at (position 1 4))))

  (check-equal? (parse script-parser "") null)

  (check-equal? (parse script-parser "foo\n") (stack "0-foo"))

  (check-equal? (parse script-parser "foo\nbar\n") (stack "0-foo" "1-bar"))
  (check-equal? (parse script-parser "foo, bar\n") (stack "0-foo" "1-bar"))

  (check-equal? (parse script-parser "foo\nbar, goo\n") (stack "0-foo" "1-bar" "2-goo"))
  (check-equal? (parse script-parser "foo, bar\ngoo\n") (stack "0-foo" "1-bar" "2-goo"))

  (check-equal?
    (parse script-parser "foo\nbar\ngoo 123\nzar\n  123\n  456\n")
    (stack "0-foo" "1-bar" "2-goo+123" "3-zar+123+456"))

  (check-equal? (parse script-parser "foo")  (failure! parse-incomplete (at (position 1 4))))
)
