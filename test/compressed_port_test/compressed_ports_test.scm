(module compressed-ports-test
   (main main)
   (library btest compressed_ports))

(define (read-last-line port)
   (let loop ((curr (read-line port))
              (last ""))
      (if (eof-object? curr)
          last
          (loop (read-line port)
             curr))))

(define +org-csv-last-line+
   "2000000,59bF9e17EfBbbaE,Hoffman Ltd,https://www.simpson.org/,Russian Federation,Future-proofed homogeneous installation,2002,Government Relations,2271" )


(define-test-suite compressed-ports-suite)


(define-test-suite zlib-suite

   (test "file-gzip? works"
      (assert-true (file-gzip? "test_files/orgs.csv.gz"))
      (assert-false (file-gzip? "test_files/orgs.csv.zstd")))

   (test "gzip compressing and decompressing returns orginal value"
      (let* ((test-string "a string to compress yay yay yay")
            (out-string (open-output-string))
            (output (output-port->gzip-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (input-port->gzip-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from gzip file works"
      (let* ((input (open-input-gzip-file "test_files/orgs.csv.gz"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input)))

   (test "file-deflate? works"
      (assert-true (file-deflate? "test_files/orgs.csv.deflate"))
      (assert-false (file-deflate? "test_files/orgs.csv.zstd")))

   (test "deflating and inflating returns orginal value"
      (let* ((test-string "a string to compress yay yay yay")
            (out-string (open-output-string))
            (output (output-port->deflate-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (input-port->inflate-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from deflate file works"
      (let* ((input (open-input-inflate-file "test_files/orgs.csv.deflate"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input)))

   (test "file-zlib? works"
      (assert-true (file-zlib? "test_files/orgs.csv.zlib"))
      (assert-false (file-zlib? "test_files/orgs.csv.zstd")))
   
   (test "zlib compressing and decompressing returns orginal value"
      (let* ((test-string "a string to compress yay yay yay")
            (out-string (open-output-string))
            (output (output-port->zlib-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (input-port->zlib-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from zlib file works"
      (let* ((input (open-input-zlib-file "test_files/orgs.csv.zlib"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input)))

   )


(define-test-suite zstd-suite

   (test "file-zstd? works"
      (assert-true (file-zstd? "test_files/orgs.csv.zstd"))
      (assert-false (file-zstd? "test_files/orgs.csv.gz")))

   
   (test "zstd compressing and decompressing returns orginal value"
      (let* ((test-string "a string to compress yay yay yay")
            (out-string (open-output-string))
            (output (output-port->zstd-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (input-port->zstd-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from zstd file works"
      (let* ((input (open-input-zstd-file "test_files/orgs.csv.zstd"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input))))


(define-test-suite xz-suite
   
   (test "file-xz? works"
      (assert-true (file-xz? "test_files/orgs.csv.xz"))
      (assert-false (file-xz? "test_files/orgs.csv.gz")))
   
   (test "xz compressing and decompressing returns orginal value"
      (let* ((test-string "a string to compress yay yay yay")
            (out-string (open-output-string))
            (output (output-port->xz-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (input-port->xz-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from xz file works"
      (let* ((input (open-input-xz-file "test_files/orgs.csv.xz"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input))))


(suite-add-subsuite! compressed-ports-suite zlib-suite)
(suite-add-subsuite! compressed-ports-suite zstd-suite)
(suite-add-subsuite! compressed-ports-suite xz-suite)

(define (main args)

   (let ((tr (instantiate::terminal-test-runner (suite compressed-ports-suite))))
      (test-runner-execute tr #t)))


