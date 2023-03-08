((let-values (((point4)
               (vector
                "Michał"
                20
                #t))
              ((text3) "foo")
              ((text2) "bar"))
   (string-append
    (string-append text2 text2)
    (unsafe-vector-ref
     point4
     0))))
