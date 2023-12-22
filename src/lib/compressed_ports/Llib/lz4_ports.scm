(module compressed-ports/lz4-ports
   (import compressed-ports/java-port-interop)
   (extern
      (include "bgl_lz4.h")

      (type lz4-compress-stream*
         (opaque)
         "struct lz4_compress_stream*")
      
      (type lz4-decompress-stream*
         (opaque)
         "struct lz4_decompress_stream*")

      (macro $lz4-chunk-size::long
         "LZ4_CHUNK_SIZE")
      
      (macro $bgl-lz4-init::void ()
             "bgl_lz4_init")

      (macro $bgl-lz4-create-compress-stream::lz4-compress-stream*
         (output::output-port) "bgl_lz4_create_compress_stream")
      
      (macro $bgl-lz4-create-decompress-stream::lz4-decompress-stream*
         () "bgl_lz4_create_decompress_stream")

      (macro $bgl-lz4-stream-compress::obj (stream::lz4-compress-stream*
                                              buffer::string
                                              length::long
                                              finish?::bbool)
             "bgl_lz4_stream_compress")
      
      (macro $bgl-lz4-stream-decompress::obj (stream::lz4-decompress-stream*
                                              buffer::string
                                              length::long)
             "bgl_lz4_stream_decompress")

      (macro $bgl-lz4-close-compress-stream::obj
         (stream::lz4-compress-stream*)
         "bgl_lz4_close_compress_stream")
      
      (macro $bgl-lz4-close-decompress-stream::obj
         (stream::lz4-decompress-stream*)
         "bgl_lz4_close_decompress_stream"))

   (java
      (class $lz4-input-stream::$input-stream
         (constructor create (::$input-stream))
         "net.jpountz.lz4.LZ4FrameInputStream")
      (class $lz4-output-stream::$output-stream
         (constructor create (::$output-stream))
         "net.jpountz.lz4.LZ4FrameOutputStream"))

   (export
      (input-port->lz4-port::input-port port::input-port #!optional (bufinfo #t))
      (open-input-lz4-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
      (file-lz4?::bbool file::bstring)
      (open-output-lz4-file file-name::bstring #!optional (bufinfo #t))
      (open-input-lz4-port::input-port in::input-port #!optional (bufinfo #t))
      (output-port->lz4-port::output-port port::output-port #!optional (bufinfo #t))
      (open-output-lz4-port out::output-port #!optional (bufinfo #t)))

   (option
      (set! *inlining?* #f)))



;; make sure we initialize lz4 as a part of module initialization
(cond-expand
   (bigloo-c
    ($bgl-lz4-init)))


(cond-expand
   (bigloo-c
    (define (output-port->lz4-port::output-port port::output-port #!optional (bufinfo #t))
       (let* ((stream ($bgl-lz4-create-compress-stream port))
             (writeproc (lambda (s)
                           ($bgl-lz4-stream-compress stream s (string-length s) #f)))
             (close (lambda ()
                       ($bgl-lz4-stream-compress stream "" 0 #t)
                       ($bgl-lz4-close-compress-stream stream)
                       #t))
             (lz4-port (open-output-procedure writeproc
                          (lambda () #f) bufinfo close)))       
          lz4-port)))
   (bigloo-jvm
    (define (output-port->lz4-port::output-port port::output-port #!optional (bufinfo #t))
       (let* ((output-port-stream ($output-port-stream-create port))
                (stream ($lz4-output-stream-create output-port-stream))
                (writeproc (lambda (s)
                             ($output-stream-write stream s)))
                (close (lambda ()
                         ($output-stream-close stream)
                         #t))
                (lz4-port (open-output-procedure writeproc
                            (lambda () #f) bufinfo close)))
            lz4-port))))

(cond-expand
      (bigloo-c
       (define (input-port->lz4-port::input-port port::input-port #!optional (bufinfo #t))
          (let* ((stream ($bgl-lz4-create-decompress-stream))
                 (buffer-size $lz4-chunk-size)
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
                                             ($bgl-lz4-stream-decompress stream
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
                                                 (proc "lz4-input-proc")
                                                 (msg "unknown input state")
                                                 (obj res)))))))))
                 (lz4-port (open-input-procedure proc bufinfo)))
             
             (input-port-close-hook-set! lz4-port
                (lambda (p) ($bgl-lz4-close-decompress-stream stream)
                        #unspecified))
             lz4-port)))
      (bigloo-jvm
       (define (input-port->lz4-port::input-port port::input-port #!optional (bufinfo #t))
          (let* ((input-port-stream ($input-port-stream-create port))
                 (stream ($lz4-input-stream-create input-port-stream))
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
                                                 (proc "lz4-input-proc")
                                                 (msg "unknown input state")
                                                 (obj bytes-read)))))))))
                 (lz4-port (open-input-procedure proc bufinfo)))
             
             (input-port-close-hook-set! lz4-port
                (lambda (p) ($input-stream-close stream)
                        #unspecified))
             lz4-port))))


(define (open-output-lz4-file file-name::bstring #!optional (bufinfo #t))
   (let ((out (open-output-file file-name bufinfo)))
      (if (output-port? out)
          (let ((output-port (output-port->lz4-port out)))
             (output-port-close-hook-set! output-port
                (lambda (p) (close-output-port out)))
             output-port))))


(define (open-input-lz4-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))

   (let ((in (open-input-file file-name bufinfo timeout)))
      (if (input-port? in)
          (let ((input-port (input-port->lz4-port in)))
             (input-port-close-hook-set! input-port
                (lambda (p) (close-input-port in)))
             input-port))))

(define (file-lz4?::bbool file::bstring)
   (with-handler (lambda (e)
                    #f)
                 (let ((input (open-input-lz4-file file)))
                    (if (input-port? input)
                        (unwind-protect
                           (begin (read-byte input)
                                  #t)
                           (close-input-port input))))))

(define (open-input-lz4-port in::input-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-input-lz4-port" bufinfo c-default-io-bufsiz)))
      (input-port->lz4-port in buf)))

(define (open-output-lz4-port out::output-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-output-lz4-port" bufinfo c-default-io-bufsiz)))
      (output-port->lz4-port out buf)))



