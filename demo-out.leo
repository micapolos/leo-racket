((let-values (((recipe4)
               (lambda (number3 text2)
                 (string-append
                  (string-append
                   (number->string (+ number3 1))
                   " ")
                  text2))))
   (recipe4 10 "jajek")))
