(module compressed-ports/xz-ports
   (import compressed-ports/java-port-interop)
   (extern
      (include "bgl_xz.h")
      
      (type xz-compress-stream*
         (opaque)
         "struct xz_compress_stream *")

      (type xz-decompress-stream*
         (opaque)
         "struct xz_decompress_stream *")

      (macro $bgl-xz-init::void ()
             "bgl_xz_init")
      
      (macro $bgl-xz-create-compress-stream::xz-compress-stream*
         (preset::int output::output-port)
         "bgl_xz_create_compress_stream")

      (macro $bgl-xz-create-decompress-stream::xz-decompress-stream*
         () "bgl_xz_create_decompress_stream")

      (macro $bgl-xz-stream-compress::obj (stream::xz-compress-stream*
                                      buffer::string
                                      length::long
                                      finish?::bbool)
             "bgl_xz_stream_compress")

      (macro $bgl-xz-stream-decompress::obj (stream::xz-decompress-stream*
                                              buffer::string
                                              length::long)
             "bgl_xz_stream_decompress")

      (macro $bgl-xz-close-input-stream::obj
         (stream::xz-decompress-stream*)
         "bgl_xz_close_input_stream")

      (macro $bgl-xz-close-output-stream::obj
         (stream::xz-compress-stream*)
         "bgl_xz_close_output_stream"))

   (java
      (abstract-class $filter-options
         "org.tukaani.xz.FilterOptions")
      (class $lzma2-options::$filter-options
         (constructor create ())
         "org.tukaani.xz.LZMA2Options")
      (class $xz-input-stream::$input-stream
         (constructor create (::$input-stream))
         "org.tukaani.xz.XZInputStream"
         )
      (class $xz-output-stream::$output-stream
         (constructor create (::$output-stream ::$filter-options))
         "org.tukaani.xz.XZOutputStream"))
   (export
      (output-port->xz-port::output-port port::output-port)
      (input-port->xz-port::input-port port::input-port #!optional (bufinfo #t))
      (open-output-xz-file file-name::bstring
           #!optional (bufinfo #t))
      (open-input-xz-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
      (file-xz?::bbool file::bstring)))


;; make sure we initialize xz as a part of module initialization
(cond-expand
   (bigloo-c
    ($bgl-xz-init)))



(cond-expand
   (bigloo-c
    (define (output-port->xz-port::output-port port::output-port)
       (let* ((stream ($bgl-xz-create-compress-stream 6 port))
             (writeproc (lambda (s)
                           ($bgl-xz-stream-compress stream s (string-length s) #f)))
             (close (lambda ()
                       ($bgl-xz-stream-compress stream "" 0 #t)
                       ($bgl-xz-close-output-stream stream)
                       #t))
             (xz-port (open-output-procedure writeproc (lambda () #f) #t close)))       
         xz-port)))
   (bigloo-jvm
    (define (output-port->xz-port::output-port port::output-port)
         (let* ((output-port-stream ($output-port-stream-create port))
                (stream ($xz-output-stream-create output-port-stream
                           ($lzma2-options-create)))
              (writeproc (lambda (s)
                            ($output-stream-write stream s)))
              (close (lambda ()
                        ($output-stream-close stream)
                        #t))
              (xz-port (open-output-procedure writeproc
                            (lambda () #f) #t close)))
            xz-port))))


(cond-expand
   (bigloo-c
    (define (input-port->xz-port::input-port port::input-port #!optional (bufinfo #t))
       (let* ((stream ($bgl-xz-create-decompress-stream))
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
                                          ($bgl-xz-stream-decompress stream
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
                                                        (proc "xz-input-proc")
                                                        (msg "unknown input state")
                                                        (obj res)))))))))
              (xz-port (open-input-procedure proc bufinfo)))

          (input-port-close-hook-set! xz-port
             (lambda (p) ($bgl-xz-close-input-stream stream)
                     #unspecified))
          xz-port)))
   (bigloo-jvm
    (define (input-port->xz-port::input-port port::input-port #!optional (bufinfo #t))
       (let* ((input-port-stream ($input-port-stream-create port))
                      (stream ($xz-input-stream-create input-port-stream))
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
                                                      (proc "xz-input-proc")
                                                      (msg "unknown input state")
                                                      (obj bytes-read)))))))))
                      (xz-port (open-input-procedure proc bufinfo)))
        
          (input-port-close-hook-set! xz-port
             (lambda (p) ($input-stream-close stream)
                     #unspecified))
          xz-port))))


(define (open-output-xz-file file-name::bstring
           #!optional (bufinfo #t))
   (let ((out (open-output-file file-name bufinfo)))
      (if (output-port? out)
          (let ((output-port (output-port->xz-port out)))
             (output-port-close-hook-set! output-port
                (lambda (p) (close-output-port out)))
             output-port))))


(define (open-input-xz-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))

   (let ((in (open-input-file file-name bufinfo timeout)))
      (if (input-port? in)
          (let ((input-port (input-port->xz-port in)))
             (input-port-close-hook-set! input-port
                (lambda (p) (close-input-port in)))
             input-port))))

(define (file-xz?::bbool file::bstring)
   (with-handler (lambda (e)
                    #f)
                 (let ((input (open-input-xz-file file)))
                    (if (input-port? input)
                        (unwind-protect
                           (begin (read-byte input)
                                  #t)
                           (close-input-port input))))))
