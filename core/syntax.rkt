#lang racket/base

(provide (all-defined-out))

(require 
  rackunit
  racket/string
  (for-syntax racket/base))

(struct leo (reversed-statement-stxs reversed-value-stxs) #:transparent)

(define empty-leo (leo null null))

(define (leo-stxs $leo)
  (reverse
    (append
      (leo-reversed-value-stxs $leo)
      (leo-reversed-statement-stxs $leo))))

(define (leo-append $lhs $rhs) 
  (struct-copy leo $lhs
    (reversed-value-stxs
      (cond
        ((null? (leo-reversed-statement-stxs $rhs))
          (append
            (leo-reversed-value-stxs $rhs)
            (leo-reversed-value-stxs $lhs)))
        (else
          (cons
            #`(begin-values ,@(leo-stxs $rhs))
            (leo-reversed-value-stxs $lhs)))))))

; -------------------------------------------------------

(define (symbol-colon-suffix? $symbol)
  (string-suffix? (symbol->string $symbol) ":"))

(define (symbol-drop-last-char $symbol)
  (string->symbol
    (let (($string (symbol->string $symbol)))
      (substring $string 0 (- (string-length $string) 1)))))

(define (stx-symbol-drop-last-char $stx)
  (datum->syntax $stx (symbol-drop-last-char (syntax-e $stx))))

; -------------------------------------------------------

(define (skip-char-count $port $count)
  (read-string $count $port))

(define (skip-char $port)
  (read-char $port))

; -------------------------------------------------------

(define (peek-eof $port ($skip 0))
  (eof-object? (peek-char $port $skip)))

(check-equal? (peek-eof (open-input-string "")) #t)
(check-equal? (peek-eof (open-input-string "a")) #f)

; -------------------------------------------------------

(define (peek-exact-string $port $string ($skip 0))
  (equal? 
    (peek-string (string-length $string) $skip $port)
    $string))

(check-equal? (peek-exact-string (open-input-string "") "") #t)
(check-equal? (peek-exact-string (open-input-string "a") "") #t)
(check-equal? (peek-exact-string (open-input-string "a") "a") #t)
(check-equal? (peek-exact-string (open-input-string "a") "b") #f)
(check-equal? (peek-exact-string (open-input-string "ab") "a") #t)
(check-equal? (peek-exact-string (open-input-string "ab") "ab") #t)
(check-equal? (peek-exact-string (open-input-string "ab") "ac") #f)
(check-equal? (peek-exact-string (open-input-string "abc") "ab") #t)

; -------------------------------------------------------

(define indent-string "  ")
(define indent-length (string-length indent-string))

(define (peek-indent $port ($skip 0)) 
  (peek-exact-string $port indent-string $skip))

; -------------------------------------------------------

(define (peek-depth-max $port ($max -1) ($depth 0) ($skip 0))
  (cond
    ((= $max 0) $depth)
    ((peek-indent $port $skip) 
      (peek-depth-max $port (- $max 1) (+ $depth 1) (+ indent-length $skip)))
    (else $depth)))

(check-equal? (peek-depth-max (open-input-string "") 2) 0)
(check-equal? (peek-depth-max (open-input-string " ") 2) 0)
(check-equal? (peek-depth-max (open-input-string "  ") 2) 1)
(check-equal? (peek-depth-max (open-input-string "   ") 2) 1)
(check-equal? (peek-depth-max (open-input-string "    ") 2) 2)
(check-equal? (peek-depth-max (open-input-string "     ") 2) 2)
(check-equal? (peek-depth-max (open-input-string "      ") 2) 2)

; -------------------------------------------------------

(define (peek-exact-depth $port $depth)
  (= (peek-depth-max $port $depth) $depth))

(define (skip-depth $port $depth)
  (skip-char-count $port (* indent-length $depth)))

; -------------------------------------------------------

(define (read-depth $port ($max -1) ($depth 0))
  (cond
    ((= $max 0) $depth)
    ((peek-indent $port) 
      (skip-char-count $port indent-length)
      (read-depth $port (- $max 1) (+ $depth 1)))
    (else $depth)))

(check-equal? (read-depth (open-input-string "")) 0)
(check-equal? (read-depth (open-input-string " ")) 0)
(check-equal? (read-depth (open-input-string " a")) 0)
(check-equal? (read-depth (open-input-string "  ")) 1)
(check-equal? (read-depth (open-input-string "   ")) 1)
(check-equal? (read-depth (open-input-string "   a")) 1)
(check-equal? (read-depth (open-input-string "    ")) 2)
(check-equal? (read-depth (open-input-string "    a") 0) 0)
(check-equal? (read-depth (open-input-string "    a") 1) 1)
(check-equal? (read-depth (open-input-string "    a") 2) 2)
(check-equal? (read-depth (open-input-string "    a") 3) 2)

; -------------------------------------------------------

(define (read-atom $port $src) 
  (read-syntax $src $port))

(define (read-reversed-atoms $port $src $reversed-atoms)
  (leo-reversed-value-stxs 
    (read-leo-atoms $port $src (leo null $reversed-atoms))))

(define (read-leo-atoms $port $src $leo)
  (let (($leo (leo null (cons (read-atom $port $src) (leo-reversed-value-stxs $leo)))))
    (cond
      ((equal? (peek-char $port) #\space)
        (skip-char $port)
        (read-leo-atoms $port $src $leo))
      ((equal? (peek-char $port) #\newline)
        (skip-char $port)
        $leo)
      (else (error "expected space or newline after atoms")))))

(define (read-atoms $port $src)
  (reverse (read-reversed-atoms $port $src null)))

; -------------------------------------------------------

(define (read-leo-syntaxes $port ($src #f) ($depth 0))
  (reverse (read-leo-reverse-syntaxes $port $src $depth null)))

(define (read-leo-list-syntaxes $port ($src #f) ($depth 0))
  (reverse (read-leo-reverse-list-syntaxes $port $src $depth null)))

(define (read-leo-syntaxes-once $port ($src #f) ($depth 0))
  (reverse (read-leo-reverse-syntaxes-once $port $src $depth null)))

(define (read-leo-reverse-syntaxes $port $src $depth $reversed-stxs)
  (leo-reversed-value-stxs (read-leo $port $src $depth (leo null $reversed-stxs))))

(define (read-leo $port ($src "") ($depth 0) ($leo empty-leo))
  (cond
    ((peek-eof $port) $leo)
    (else
      (let*
        (($reversed-combined-stxs
          (read-leo-reverse-syntaxes-once $port $src $depth (leo-reversed-value-stxs $leo))))
        (cond
          ((peek-exact-depth $port $depth)
            (skip-depth $port $depth)
            (leo null (read-leo-reverse-syntaxes $port $src $depth $reversed-combined-stxs)))
          (else (leo null $reversed-combined-stxs)))))))

(define (read-leo-reverse-list-syntaxes $port $src $depth $reversed-stxs)
  (leo-reversed-value-stxs (read-leo-list $port $src $depth (leo null $reversed-stxs))))

(define (read-leo-list $port ($src "") ($depth 0) ($leo empty-leo))
  (cond
    ((peek-eof $port) $leo)
    (else
      (let*
        (($reversed-line-stxs
          (read-leo-reverse-syntaxes-once $port $src $depth null)))
        (cond
          ((peek-exact-depth $port $depth)
            (skip-depth $port $depth)
            (leo null
              (read-leo-reverse-list-syntaxes $port $src $depth
                (append 
                  $reversed-line-stxs 
                  (leo-reversed-value-stxs $leo)))))
          (else 
            (leo null 
              (append 
                $reversed-line-stxs 
                (leo-reversed-value-stxs $leo)))))))))

(define (read-leo-reverse-syntaxes-once $port $src $depth $reversed-stxs)
  (leo-reversed-value-stxs (read-leo-line $port $src $depth (leo null $reversed-stxs))))

(define (read-leo-line $port $src $depth $leo)
  (cond
    ((equal? (peek-char $port) #\newline)
      (skip-char $port)
      (read-leo-line $port $src $depth $leo))
    ((eof-object? (peek-char $port)) empty-leo)
    ((char-whitespace? (peek-char $port))
      (error "leo can not start with whitespace"))
    (else
      (let* (($stx (read-atom $port $src))
             ($datum (syntax-e $stx)))
        (cond
          ((equal? $datum `do)
            (read-leo-do-rhs $port $src $depth $leo))
          ((equal? $datum `do:)
            (read-leo-do-colon-rhs $port $src $depth $leo))
          ((equal? $datum `the)
            (read-leo-the-rhs $port $src $depth $leo))
          ((equal? $datum `the:)
            (read-leo-the-colon-rhs $port $src $depth $leo))
          ; TODO datum:
          ((symbol? $datum)
            (cond
              ((symbol-colon-suffix? $datum)
                (read-leo-symbol-colon-rhs $port $src $depth $leo
                  (stx-symbol-drop-last-char $stx)))
              (else 
                (read-leo-symbol-rhs $port $src $depth $leo $stx))))
          (else
            (read-leo-default-line $port $src $depth $leo $stx)))))))

(define (read-leo-rhs-gather-syntaxes $port $src $depth $reversed-lhs-stxs)
  (cond
    ((peek-exact-string $port " ")
      (skip-char $port)
      (append
        (read-leo-reverse-syntaxes $port $src (+ $depth 1) null)
        $reversed-lhs-stxs))
    (else (error "expected space after -"))))

(define (read-leo-do-rhs $port $src $depth $leo)
  (leo-append $leo (read-leo-rhs $port $src $depth)))

(define (read-leo-do-colon-rhs $port $src $depth $leo)
  (leo-append $leo (read-leo-rhs-atoms $port $src)))

(define (read-leo-rhs-the-syntaxes $port $src $depth $reversed-lhs-stxs)
  (cons
    #`(#,@(read-leo-rhs-syntaxes $port $src $depth))
    $reversed-lhs-stxs))

(define (read-leo-the-rhs $port $src $depth $leo)
  (leo-append 
    $leo 
    (leo null (list #`(#,@(read-leo-rhs-syntaxes $port $src $depth))))))

(define (read-leo-the-colon-rhs $port $src $depth $leo)
  (leo-append
    $leo
    (leo null (list #`(#,@(reverse (read-rhs-reversed-atoms $port $src)))))))

(define (read-leo-symbol-rhs $port $src $depth $leo $symbol)
  (let 
    (($args
      (reverse
        (append
          (leo-reversed-value-stxs (read-leo-rhs $port $src $depth))
          (leo-reversed-value-stxs $leo)))))
    (cond
      ((null? $args) (leo null (list $symbol)))
      (else (leo null (list #`(#,$symbol #,@$args)))))))

(define (read-leo-rhs-colon-symbol-syntaxes $port $src $depth $reversed-lhs-stxs $symbol)
  (let 
    (($args 
      (append
        (reverse $reversed-lhs-stxs)
        (read-leo-rhs-list-syntaxes $port $src $depth))))
    (list #`(#,$symbol #,@$args))))

(define (read-leo-symbol-colon-rhs $port $src $depth $leo $symbol)
  (let 
    (($args 
      (append
        (reverse (leo-reversed-value-stxs $leo))
        (read-leo-rhs-list-syntaxes $port $src $depth))))
    (leo null (list #`(#,$symbol #,@$args)))))

(define (read-leo-default-line $port $src $depth $leo $default)
  (cond 
    ((equal? (peek-char $port) #\newline)
      (skip-char $port)
      (leo null (list $default)))
    (else (error "expected newline after datum"))))

(define (read-leo-rhs-syntaxes $port $src $depth)
  (leo-stxs (read-leo-rhs $port $src $depth)))

(define (read-leo-rhs $port $src $depth)
  (let (($char (peek-char $port)))
    (cond
      ((equal? $char #\space)
        (skip-char $port)
        (read-leo-line $port $src $depth empty-leo))
      ((equal? $char #\newline)
        (skip-char $port)
        (let (($rhs-depth (+ $depth 1)))
          (cond
            ((peek-exact-depth $port $rhs-depth)
              (skip-depth $port $rhs-depth)
              (read-leo $port $src $rhs-depth empty-leo))
            (else empty-leo))))
      (else 
        (error "expected space or newline before rhs")))))

(define (read-rhs-reversed-atoms $port $src)
  (leo-reversed-value-stxs (read-leo-rhs-atoms $port $src)))

(define (read-leo-rhs-atoms $port $src)
  (let (($char (peek-char $port)))
    (cond
      ((equal? $char #\space)
        (skip-char $port)
        (read-leo-atoms $port $src empty-leo))
      ((equal? $char #\newline)
        (skip-char $port)
        empty-leo)
      (else 
        (error "expected space before rhs atoms")))))

(define (read-leo-rhs-list-syntaxes $port $src $depth)
  (let (($char (peek-char $port)))
    (cond
      ((equal? $char #\space)
        (skip-char $port)
        (read-atoms $port $src))
      ((equal? $char #\newline)
        (skip-char $port)
        (let (($rhs-depth (+ $depth 1)))
          (cond
            ((peek-exact-depth $port $rhs-depth)
              (skip-depth $port $rhs-depth)
              (read-leo-list-syntaxes $port $src $rhs-depth))
            (else null))))
      (else 
        (error "expected space or newline before rhs")))))

(define (string->leo-syntaxes $string)
  (leo-stxs (read-leo (open-input-string $string))))

(define (string->leo-datums $string)
  (map syntax->datum (string->leo-syntaxes $string)))

(check-equal? (string->leo-datums "") `())

(check-equal? (string->leo-datums "foo\n") `(foo))

(check-equal? (string->leo-datums "\nfoo\n") `(foo))

(check-equal? (string->leo-datums "foo\nbar\n") `((bar foo)))
(check-equal? (string->leo-datums "foo\nbar\nzoo\n") `((zoo (bar foo))))

(check-equal? (string->leo-datums "foo bar\n") `((foo bar)))
(check-equal? (string->leo-datums "foo bar zoo\n") `((foo (bar zoo))))

(check-equal? (string->leo-datums "foo\n  bar\n") `((foo bar)))
(check-equal? (string->leo-datums "foo\n  bar\n  zoo\n") `((foo (zoo bar))))

(check-equal? (string->leo-datums "foo bar\n  zoo\n") `((foo (bar zoo))))
(check-equal? (string->leo-datums "foo\n  bar zoo\n") `((foo (bar zoo))))

(check-equal? (string->leo-datums "foo 123\n") `((foo 123)))
(check-equal? (string->leo-datums "1\nplus 2\n") `((plus 1 2)))
(check-equal? (string->leo-datums "\"foo\"\n") `("foo"))

(check-equal? 
  (string->leo-datums "1\nplus 2\ntimes\n  3\n  minus 4\n") 
  `((times (plus 1 2) (minus 3 4))))

(check-equal? (string->leo-datums "do\n") `())
(check-equal? (string->leo-datums "do 1\n") `(1))
(check-equal? (string->leo-datums "1\ndo\n") `(1))
(check-equal? (string->leo-datums "1\ndo 2\n") `(1 2))
(check-equal? (string->leo-datums "do 1\ndo 2\n") `(1 2))
(check-equal? (string->leo-datums "do 1\n") `(1))
(check-equal? (string->leo-datums "do\n  1\n  plus 2\ndo\n  3\n  plus 4\n") `((plus 1 2) (plus 3 4)))

(check-equal? (string->leo-datums "do:\n") `())
(check-equal? (string->leo-datums "do: 1\n") `(1))
(check-equal? (string->leo-datums "do: 1 2\n") `(1 2))

(check-equal? 
  (string->leo-datums "1\nplus 2\ndo\n  3\n  minus 4\n") 
  `((plus 1 2) (minus 3 4)))

(check-equal?
  (string->leo-datums "foo:\n") 
  `((foo)))

(check-equal?
  (string->leo-datums "foo: 1\n") 
  `((foo 1)))

(check-equal?
  (string->leo-datums "foo: 1 2\n") 
  `((foo 1 2)))

(check-equal?
  (string->leo-datums "foo:\n  x 1\n") 
  `((foo (x 1))))

(check-equal?
  (string->leo-datums "foo:\n  x 1\n  y 2\n") 
  `((foo (x 1) (y 2))))

(check-equal?
  (string->leo-datums "circle:\n  radius 10\n  center:\n    x 10\n    y 20\n") 
  `((circle (radius 10) (center (x 10) (y 20)))))

(check-equal? (string->leo-datums "the\n") `(()))
(check-equal? (string->leo-datums "the:\n") `(()))

(check-equal? (string->leo-datums "the: 1\n") `((1)))
(check-equal? (string->leo-datums "the: 1 2\n") `((1 2)))

(check-equal? (string->leo-datums "the\n  1\n") `((1)))
(check-equal? (string->leo-datums "the\n  foo\n  bar\n") `(((bar foo))))

(check-equal? (string->leo-datums "the foo bar zoo\n") `(((foo (bar zoo)))))

; -------------------------------------------------------------

(define (string->leo-list-syntaxes $string)
  (leo-stxs (read-leo-list (open-input-string $string))))

(define (string->leo-list-datums $string)
  (map syntax->datum (string->leo-list-syntaxes $string)))

(check-equal? (string->leo-list-datums "") `())
(check-equal? (string->leo-list-datums "1\n") `(1))
(check-equal? (string->leo-list-datums "1\n2\n") `(1 2))
(check-equal? (string->leo-list-datums "do\n  1\n  + 2\n") `((+ 1 2)))
