#lang leo/typed

(require
  leo/parser/literal-parser)

(data (V) env
  (atom-parser-fn : (-> V (Parser V)))
  (begin-parser-fn : (-> V Symbol (-> V (-> V V) (Parser V)) (Parser V))))

(: env-atom-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-atom-parser $env $value)
  (#%app (env-atom-parser-fn $env) $value))

(: env-begin-parser : (All (V) (-> (Env V) V Symbol (-> V (-> V V) (Parser V)) (Parser V))))
(define (env-begin-parser $env $value $symbol $inner-parser-fn)
  (#%app (env-begin-parser-fn $env) $value $symbol $inner-parser-fn))

; --------------------------------------------------------------------------------------

(define comma-separator-parser
  (exact-string-parser ", "))

(define newlines-separator-parser
  newlines-parser)

(define comma-or-newlines-separator-parser
  (parser-or
    comma-separator-parser
    newlines-separator-parser))

(: env-script-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-script-parser $env $value)
  (prefix-parser maybe-newlines-parser
    (parser-or
      (parser $value)
      (parser-suffix
        (repeat-separated-parser $value comma-or-newlines-separator-parser
          (lambda (($repeated-value : V))
            (env-line-parser $env $repeated-value)))
        newlines-parser))))

(: env-line-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-line-parser $env $value)
  (parser-or
    (env-atom-line-parser $env $value)
    (env-sentence-parser $env $value)))

(: env-atom-line-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-atom-line-parser $env $value)
  (parser-bind
    (env-atom-parser $env $value)
    (lambda (($atom : V))
      (parser-or
        (parser $atom)
        (prefix-parser (exact-char-parser #\.)
          (env-sentence-parser $env $atom))))))

(: env-sentence-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-sentence-parser $env $value)
  (parser-bind word-parser
    (lambda (($word : Word))
      (env-rhs-parser $env $value
        (word-symbol $word)))))

(: env-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-rhs-parser $env $value $symbol)
  (parser-or
    (env-symbol-parser $env $value $symbol)
    (env-space-rhs-parser $env $value $symbol)
    (env-colon-rhs-parser $env $value $symbol)
    (env-parens-rhs-parser $env $value $symbol)
    (env-newline-rhs-parser $env $value $symbol)
    (env-dot-rhs-parser $env $value $symbol)))

(: env-symbol-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-symbol-parser $env $value $symbol)
  (env-begin-parser $env $value $symbol
    (lambda (($rhs : V) ($end-fn : (-> V V))) : (Parser V)
      (parser (#%app $end-fn $rhs)))))

(: env-space-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-space-rhs-parser $env $value $symbol)
  (prefix-parser space-parser
    (env-begin-parser $env $value $symbol
      (lambda (($rhs : V) ($end-fn : (-> V V))) : (Parser V)
        (parser-map
          (env-line-parser $env $rhs)
          (lambda (($parsed-rhs : V))
            (#%app $end-fn $parsed-rhs)))))))

(: env-colon-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-colon-rhs-parser $env $value $symbol)
  (prefix-parser (exact-string-parser ": ")
    (env-begin-parser $env $value $symbol
      (lambda (($rhs : V) ($end-fn : (-> V V))) : (Parser V)
        (parser-map
          (repeat-separated-parser $rhs comma-separator-parser
            (lambda (($repeated-rhs : V))
              (env-line-parser $env $repeated-rhs)))
          (lambda (($parsed-rhs : V))
            (#%app $end-fn $parsed-rhs)))))))

(: env-parens-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-parens-rhs-parser $env $value $symbol)
  (prefix-parser-suffix
    (exact-string-parser "(")
    (env-begin-parser $env $value $symbol
      (lambda (($rhs : V) ($end-fn : (-> V V))) : (Parser V)
        (parser-or
          (parser (#%app $end-fn $rhs))
          (parser-map
            (repeat-separated-parser $rhs comma-separator-parser
              (lambda (($repeated-rhs : V))
                (env-line-parser $env $repeated-rhs)))
            (lambda (($parsed-rhs : V))
              (#%app $end-fn $parsed-rhs))))))
    (exact-string-parser ")")))

(: env-newline-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-newline-rhs-parser $env $value $symbol)
  (prefix-parser newlines-parser
    (indented-parser
      (env-begin-parser $env $value $symbol
        (lambda (($rhs : V) ($end-fn : (-> V V))) : (Parser V)
          (parser-map
            (repeat-separated-parser $rhs comma-or-newlines-separator-parser
              (lambda (($repeated-rhs : V))
                (env-line-parser $env $repeated-rhs)))
            (lambda (($parsed-rhs : V))
              (#%app $end-fn $parsed-rhs))))))))

(: env-dot-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-dot-rhs-parser $env $value $symbol)
  (prefix-parser (exact-char-parser #\.)
    (parser-bind
      (env-begin-parser $env $value $symbol
        (lambda (($rhs : V) ($end-fn : (-> V V))) : (Parser V)
          (parser (#%app $end-fn $rhs))))
      (lambda (($dotted : V))
        (env-sentence-parser $env $dotted)))))
