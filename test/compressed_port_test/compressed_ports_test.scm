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
            (output (open-output-gzip-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (open-input-gzip-port (open-input-string compressed-data))))
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
            (output (open-output-deflate-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (open-input-inflate-port (open-input-string compressed-data))))
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
            (output (open-output-zlib-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (open-input-zlib-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from zlib file works"
      (let* ((input (open-input-zlib-file "test_files/orgs.csv.zlib"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input)))

   )


(define-test-suite bzip2-suite

   (test "file-bzip2? works"
      (assert-true (file-bzip2? "test_files/orgs.csv.bz2"))
      (assert-false (file-bzip2? "test_files/orgs.csv.gz")))

   
   (test "bzip2 compressing and decompressing returns orginal value"
      (let* ((test-string "a string to compress yay yay yay")
            (out-string (open-output-string))
            (output (open-output-bzip2-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (open-input-bzip2-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from bzip2 file works"
      (let* ((input (open-input-bzip2-file "test_files/orgs.csv.bz2"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input))))


(define-test-suite zstd-suite

   (test "file-zstd? works"
      (assert-true (file-zstd? "test_files/orgs.csv.zstd"))
      (assert-false (file-zstd? "test_files/orgs.csv.gz")))

   
   (test "zstd compressing and decompressing returns orginal value"
      (let* ((test-string "a string to compress yay yay yay")
            (out-string (open-output-string))
            (output (open-output-zstd-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (open-input-zstd-port (open-input-string compressed-data))))
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
            (output (open-output-xz-port out-string)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (open-input-xz-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from xz file works"
      (let* ((input (open-input-xz-file "test_files/orgs.csv.xz"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input))))


(define-test-suite lz4-suite

   (test "file-lz4? works"
      (assert-true (file-lz4? "test_files/orgs.csv.lz4"))
      (assert-false (file-lz4? "test_files/orgs.csv.gz")))

   
   (test "lz4 compressing and decompressing returns orginal value"
      (let* ((test-string "a string to compress yay yay yay")
            (out-string (open-output-string))
            (output (open-output-lz4-port out-string #t)))
         (display test-string output)
         (close-output-port output)
         (let* ((compressed-data (get-output-string out-string))
               (input (open-input-lz4-port (open-input-string compressed-data))))
            (assert-equal? (read-string input) test-string)
            (close-input-port input))))
   
   (test "input from lz4 file works"
      (let* ((input (open-input-lz4-file "test_files/orgs.csv.lz4"))
             (last-line (read-last-line input)))
         (assert-equal? last-line +org-csv-last-line+)
         (close-input-port input))))


(suite-add-subsuite! compressed-ports-suite zlib-suite)
(suite-add-subsuite! compressed-ports-suite bzip2-suite)
(suite-add-subsuite! compressed-ports-suite zstd-suite)
(suite-add-subsuite! compressed-ports-suite xz-suite)
(suite-add-subsuite! compressed-ports-suite lz4-suite)

(define (main args)

   ; (let ((out (open-output-lz4-file "test.lz4")))
   ;    (do ((i 0 (+ i 1)))
   ;        ((= i 1000000))
   ;        (fprint out "a string to compress yay yay yay"))
   ;    (close-output-port out))
   
    (let ((tr (instantiate::terminal-test-runner (suite compressed-ports-suite))))
       (test-runner-execute tr #t))

    )


