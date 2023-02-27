 #lang typed/racket/base

(provide (all-defined-out))

(struct racket ((any : Any))
  #:transparent
  #:type-name Racket)
