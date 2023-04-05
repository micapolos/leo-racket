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

(define sentence-separator-parser : (Parser True)
  (parser-or
    (exact-string-parser ", ")
    newline-parser))

(: env-script-parser : (All (V I) (-> (Env V I) V (Parser V))))
(define (env-script-parser $env $value)
  (parser-or
    (parser $value)
    (parser-suffix
      (then-repeat-separated-parser
        (env-sentence-parser $env $value)
        sentence-separator-parser
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
    (env-spaced-rhs-parser $env $value $symbol)
    (env-indented-rhs-parser $env $value $symbol)))

(: env-symbol-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-symbol-parser $env $value $symbol)
  (env-begin-parser $env $value $symbol
    (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
      (parser (#%app $end-fn $item)))))

(: env-spaced-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-spaced-rhs-parser $env $value $symbol)
  (prefix-parser space-parser
    (env-begin-parser $env $value $symbol
      (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
        (parser-map
          (#%app $item-parser-fn $item)
          (lambda (($parsed-item : I))
            (#%app $end-fn $parsed-item)))))))

(: env-indented-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-indented-rhs-parser $env $value $symbol)
  (prefix-parser newline-parser
    (indented-parser
      (env-begin-parser $env $value $symbol
        (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($end-fn : (-> I V))) : (Parser V)
          (parser-map
            (then-repeat-separated-parser (#%app $item-parser-fn $item) newline-parser
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
            (parser-map (stack-parser numeric-char-parser)
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

  (check-equal? (parse script-parser "") null)

  (check-equal? (parse script-parser "foo\n") (stack "0-foo"))

  (check-equal? (parse script-parser "foo\nbar\n") (stack "0-foo" "1-bar"))
  (check-equal? (parse script-parser "foo, bar\n") (stack "0-foo" "1-bar"))

  (check-equal? (parse script-parser "foo\nbar, goo\n") (stack "0-foo" "1-bar" "2-goo"))
  (check-equal? (parse script-parser "foo, bar\ngoo\n") (stack "0-foo" "1-bar" "2-goo"))

  (check-equal?
    (parse script-parser "foo\nbar\ngoo 123\nzar\n  123\n  456\n")
    (stack "0-foo" "1-bar" "2-goo+123" "3-zar+123+456"))

  (check-equal? (parse script-parser "Foo\n")  (failure! parse-complete (at (position 1 2))))
  (check-equal? (parse script-parser "fo1\n")  (failure! parse-complete (at (position 1 4))))

  (check-equal? (parse script-parser "foo")  (failure! parse-incomplete (at (position 1 4))))
  (check-equal? (parse script-parser "foo, ") (failure! parse-incomplete (at (position 1 6))))
)
