(module compressed-ports-test
   (main main)
   (library compressed_ports))


;(print "dog food")

(define (main args)

   ; (define out (open-output-file "test.g"))

   ; (define gzip-out (output-port->deflate-port out))

   ; (fprint gzip-out "some text to compress yay yay yay!!!!")

   ; (close-output-port gzip-out)
   ; (close-output-port out)


   ; (define in (open-input-file "test.g"))

   ; (define deflate-in (input-port->inflate-port in))

   ; (let loop ((line (read-line deflate-in)))
   ;    (when (string? line)
   ;       (print line)
   ;       (loop (read-line deflate-in))))

   ; (close-input-port deflate-in)
   ; (close-input-port in)


   ; (define in (open-input-zstd-file "output.txt.zstd"))

   ; (let loop ((line (read-line in)))
   ;    (when (string? line)
   ;       (print line)
   ;       (loop (read-line in))))

   ; (close-input-port in)

   (define out (open-output-zstd-file "output.zstd"))
   (do ((i 0 (+ i 1)))
       ((= i 100000))
       (fprint out "some text to compress yay yay yay!!!!"))

   (close-output-port out)

   
   
   ; (define in (open-input-file "organizations-2000000.csv.gz"))
   ; (define gzip-in (input-port->gzip-port in))

   ; (let loop ((line (read-line gzip-in)))
   ;    (when (string? line)
   ;       (print line)
   ;       (loop (read-line gzip-in))))
   
   ; ; ; ;(print (read-string gzip-in))
   
   ; (close-input-port gzip-in) 
   ; (close-input-port in)

   ; (define out (open-output-file "test.g"))

   ; (define gzip-out (output-port->gzip-port out))

   
   ; (do ((i 0 (+ i 1)))
   ;     ((= i 10000))
   ;     (fprint gzip-out "some text to compress yay yay yay!!!!"))
   
   ; (close-output-port gzip-out)
   ; (close-output-port out)

   
   )


