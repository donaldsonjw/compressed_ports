(module compressed-ports/bzip2-ports
   (import compressed-ports/java-port-interop)
   (extern
      (include "bgl_bzip2.h")

      (type bzip2-compress-stream*
         (opaque)
         "struct bzip2_compress_stream *")
      
      (type bzip2-decompress-stream*
         (opaque)
         "struct bzip2_decompress_stream *")

      (macro $bgl-bzip2-init::void ()
             "bgl_bzip2_init")

      (macro $bgl-bzip2-create-decompress-stream::bzip2-decompress-stream*
         () "bgl_bzip2_create_decompress_stream")

      (macro $bgl-bzip2-create-compress-stream::bzip2-compress-stream*
         (block-size::long output::output-port)
         "bgl_bzip2_create_compress_stream")

      (macro $bgl-bzip2-stream-compress::obj (stream::bzip2-compress-stream*
                                                buffer::string
                                                length::long
                                                finish?::bbool)
             "bgl_bzip2_stream_compress")

      (macro $bgl-bzip2-stream-decompress::obj (stream::bzip2-decompress-stream*
                                                  buffer::string
                                                  length::long)
             "bgl_bzip2_stream_decompress")

      (macro $bgl-bzip2-close-compress-stream::obj
         (stream::bzip2-compress-stream*)
         "bgl_bzip2_close_compress_stream")
      (macro $bgl-bzip2-close-decompress-stream::obj
         (stream::bzip2-decompress-stream*)
         "bgl_bzip2_close_decompress_stream"))

   (java
      (class $bzip2-input-stream::$input-stream
         (constructor create (::$input-stream ::bool))
         "org/itadaki/bzip2/BZip2InputStream")

      (class $bzip2-output-stream::$output-stream
         (constructor create (::$output-stream))
         "org/itadaki/bzip2/BZip2OutputStream"))

   (export
      (input-port->bzip2-port::input-port port::input-port #!optional (bufinfo #t))
      (output-port->bzip2-port::output-port port::output-port)
      (open-input-bzip2-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
      (file-bzip2?::bbool file::bstring)
      (open-output-bzip2-file file-name::bstring
           #!optional (bufinfo #t)))
   )


;; make sure we initialize bzip2 as a part of module initialization
(cond-expand
   (bigloo-c
    ($bgl-bzip2-init)))



(cond-expand
   (bigloo-c
    (define (output-port->bzip2-port::output-port port::output-port)
       (let* ((stream ($bgl-bzip2-create-compress-stream 9 port))
             (writeproc (lambda (s)
                           ($bgl-bzip2-stream-compress stream s (string-length s) #f)))
             (close (lambda ()
                       ($bgl-bzip2-stream-compress stream "" 0 #t)
                       ($bgl-bzip2-close-compress-stream stream)
                       #t))
             (bzip2-port (open-output-procedure writeproc (lambda () #f) #t close)))       
         bzip2-port)))
   (bigloo-jvm
    (define (output-port->bzip2-port::output-port port::output-port)
         (let* ((output-port-stream ($output-port-stream-create port))
                (stream ($bzip2-output-stream-create output-port-stream))
                (writeproc (lambda (s)
                              ($output-stream-write stream s)))
                (close (lambda ()
                          ($output-stream-close stream)
                          #t))
                (bzip2-port (open-output-procedure writeproc
                            (lambda () #f) #t close)))
            bzip2-port))))


(cond-expand
   (bigloo-c
    (define (input-port->bzip2-port::input-port port::input-port #!optional (bufinfo #t))
       (let* ((stream ($bgl-bzip2-create-decompress-stream))
              (buffer-size (bit-lsh 1 15))
              (buffer (make-string buffer-size))
              (need-compressed-data? #t)
              (proc (lambda ()
                       (let loop ()
                          (let* ((bytes-read (if need-compressed-data?
                                                 (let ((br (read-fill-string! buffer 0 buffer-size port)))
                                                    (set! need-compressed-data? #f)
                                                    br)
                                                 0))
                                 (res (if (eof-object? bytes-read)
                                          'DECOMPRESS-DONE
                                          ($bgl-bzip2-stream-decompress stream
                                             (if (> bytes-read 0)
                                                 buffer "")
                                             bytes-read))))
                             
                             (cond ((string? res)                 
                                    res)
                                   ((eq? res 'DECOMPRESS-CONTINUE)
                                    (set! need-compressed-data? #t)
                                    (loop))
                                   ((eq? res 'DECOMPRESS-DONE)
                                              #f)
                                   (else
                                    (raise (instantiate::&io-error
                                                        (proc "bzip2-input-proc")
                                                        (msg "unknown input state")
                                                        (obj res)))))))))
              (bzip2-port (open-input-procedure proc bufinfo)))

          (input-port-close-hook-set! bzip2-port
             (lambda (p) ($bgl-bzip2-close-decompress-stream stream)
                     #unspecified))
          bzip2-port)))
   (bigloo-jvm
    (define (input-port->bzip2-port::input-port port::input-port #!optional (bufinfo #t))
       (let* ((input-port-stream ($input-port-stream-create port))
                      (stream ($bzip2-input-stream-create input-port-stream #f))
                      (buffer (make-string (bit-lsh 1 15)))
                      (proc (lambda ()     
                               (let loop ()
                                  (let* ((bytes-read ($input-stream-read stream buffer)))
                             
                                     (cond ((> bytes-read 0)                 
                                            (string-shrink! buffer bytes-read))
                                           ((= -1 bytes-read)
                                            #f)
                                           (else
                                            (raise (instantiate::&io-error
                                                      (proc "bzip2-input-proc")
                                                      (msg "unknown input state")
                                                      (obj bytes-read)))))))))
                      (bzip2-port (open-input-procedure proc bufinfo)))
        
          (input-port-close-hook-set! bzip2-port
             (lambda (p) ($input-stream-close stream)
                     #unspecified))
          bzip2-port))))


(define (open-output-bzip2-file file-name::bstring
           #!optional (bufinfo #t))
   (let ((out (open-output-file file-name bufinfo)))
      (if (output-port? out)
          (let ((output-port (output-port->bzip2-port out)))
             (output-port-close-hook-set! output-port
                (lambda (p) (close-output-port out)))
             output-port))))



(define (open-input-bzip2-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))

   (let ((in (open-input-file file-name bufinfo timeout)))
      (if (input-port? in)
          (let ((input-port (input-port->bzip2-port in)))
             (input-port-close-hook-set! input-port
                (lambda (p) (close-input-port in)))
             input-port))))

(define (file-bzip2?::bbool file::bstring)
   (with-handler (lambda (e)
                    #f)
                 (let ((input (open-input-bzip2-file file)))
                    (if (input-port? input)
                        (unwind-protect
                           (begin (read-byte input)
                                  #t)
                           (close-input-port input))))))
