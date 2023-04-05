#lang leo/typed

(require
  leo/compiler/sexp-parser)

(data (V I) env
  (literal-parser-fn : (-> V (Parser V)))
  (begin-parser-fn : (-> V Symbol (-> I (-> I (Parser I)) (-> V I V) (Parser V)) (Parser V))))

(: env-literal-parser : (All (V I) (-> (Env V I) V (Parser V))))
(define (env-literal-parser $env $value)
  (#%app (env-literal-parser-fn $env) $value))

(: env-begin-parser : (All (V I) (-> (Env V I) V Symbol (-> I (-> I (Parser I)) (-> V I V) (Parser V)) (Parser V))))
(define (env-begin-parser $env $value $symbol $item-parser-fn)
  (#%app (env-begin-parser-fn $env) $value $symbol $item-parser-fn))

; --------------------------------------------------------------------------------------

(: env-script-parser : (All (V I) (-> (Env V I) V (Parser V))))
(define (env-script-parser $env $value)
  (repeat-parser $value
    (lambda (($repeated-value : V))
      (parser-suffix
        (env-line-parser $env $repeated-value)
        newline-parser))))

(: env-line-parser : (All (V I) (-> (Env V I) V (Parser V))))
(define (env-line-parser $env $value)
  (parser-or
    (env-literal-parser $env $value)
    (env-sentence-parser $env $value)))

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
    (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($combine-fn : (-> V I V))) : (Parser V)
      (parser (#%app $combine-fn $value $item)))))

(: env-spaced-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-spaced-rhs-parser $env $value $symbol)
  (prefix-parser space-parser
    (env-begin-parser $env $value $symbol
      (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($combine-fn : (-> V I V))) : (Parser V)
        (parser-map
          (#%app $item-parser-fn $item)
          (lambda (($parsed-item : I))
            (#%app $combine-fn $value $parsed-item)))))))

(: env-indented-rhs-parser : (All (V I) (-> (Env V I) V Symbol (Parser V))))
(define (env-indented-rhs-parser $env $value $symbol)
  (prefix-parser newline-parser
    (indented-parser
      (env-begin-parser $env $value $symbol
        (lambda (($item : I) ($item-parser-fn : (-> I (Parser I))) ($combine-fn : (-> V I V))) : (Parser V)
          (parser-map
            (parser-bind
              (#%app $item-parser-fn $item)
              (lambda (($first-item : I))
                (repeat-parser $first-item
                  (lambda (($following-item : I))
                    (prefix-parser newline-parser
                      (#%app $item-parser-fn $following-item))))))
            (lambda (($parsed-item : I))
              (#%app $combine-fn $value $parsed-item))))))))

(let ()
  (define test-env : (Env (Stackof String) String)
    (env
      ; literal-parser: "#[char]" -> "[length]-literal-[char]"
      (lambda (($string-stack : (Stackof String))) : (Parser (Stackof String))
        (prefix-parser
          (exact-string-parser "#")
          (parser-map char-parser
            (lambda (($char : Char))
              (push $string-stack
                (string-append
                  (number->string (length $string-stack))
                  "-literal-"
                  (string $char)))))))
      ; begin-parser: -> "[length]-[symbol]"
      (lambda
        (
          ($string-stack : (Stackof String))
          ($symbol : Symbol)
          ($fn : (->
            String
            (-> String (Parser String))
            (-> (Stackof String) String (Stackof String))
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
          (lambda (($string-stack : (Stackof String)) ($string : String)) : (Stackof String)
            (push $string-stack $string))))))

  (define line-parser : (Parser (Stackof String))
    (env-line-parser test-env (stack "foo" "bar")))

  (define script-parser : (Parser (Stackof String))
    (env-script-parser test-env null))

  (check-equal? (parse line-parser "#a") (stack "foo" "bar" "2-literal-a"))
  (check-equal? (parse line-parser "foo") (stack "foo" "bar" "2-foo"))
  (check-equal? (parse line-parser "foo 123") (stack "foo" "bar" "2-foo+123"))
  (check-equal? (parse line-parser "foo\n  123") (stack "foo" "bar" "2-foo+123"))
  (check-equal? (parse line-parser "foo\n  123\n  456") (stack "foo" "bar" "2-foo+123+456"))

  (check-equal? (parse script-parser "") null)
  (check-equal? (parse script-parser "#a\n") (stack "0-literal-a"))

  (check-equal?
    (parse script-parser "#a\nfoo\nfoo 123\nbar\n  123\n  456\n")
    (stack "0-literal-a" "1-foo" "2-foo+123" "3-bar+123+456"))
)
