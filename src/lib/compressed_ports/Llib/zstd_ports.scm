(module compressed-ports/zstd-ports
   (import compressed-ports/java-port-interop)
   (extern
      (include "bgl_zstd.h")
      (type zstd-decompress-stream*
         (opaque)
         "struct zstd_decompress_stream *")
      (type zstd-compress-stream*
         (opaque)
         "struct zstd_compress_stream *")
      
      (macro $bgl-zstd-input-buffer-size::long ()
             "bgl_zstd_input_buffer_size")

      (macro $bgl-zstd-output-buffer-size::long ()
             "bgl_zstd_output_buffer_size") 
      
      (macro $bgl-zstd-create-decompress-stream::zstd-decompress-stream* ()
             "bgl_zstd_create_decompress_stream")

      (macro $bgl-zstd-create-compress-stream::zstd-compress-stream* (level::int output::output-port)
             "bgl_zstd_create_compress_stream")
      
      (macro $bgl-zstd-init::void () "bgl_zstd_init")

      (macro $bgl-zstd-stream-decompress::obj (zstream::zstd-decompress-stream*
                                               buffer::string
                                               length::long)
             "bgl_zstd_stream_decompress")
      
      (macro $bgl-zstd-stream-compress::obj (zstream::zstd-compress-stream*
                                               buffer::string
                                               length::long
                                               finish?::bbool)
             "bgl_zstd_stream_compress")

      (macro $bgl-zstd-close-compress-stream::obj (zstream::zstd-compress-stream*)
             "bgl_zstd_close_compress_stream")
      (macro $bgl-zstd-close-decompress-stream::obj (zstream::zstd-decompress-stream*)
             "bgl_zstd_close_decompress_stream"))

   (java      
      (class $zstd-input-stream::$input-stream
         (constructor create (::$input-stream))
         "com.github.luben.zstd.ZstdInputStream")
      (class $zstd-output-stream::$output-stream
         (constructor create (::$output-stream ::long))
         "com.github.luben.zstd.ZstdOutputStream"))
   

   (export
      (input-port->zstd-port::input-port port::input-port
         #!optional (bufinfo #t))
      (output-port->zstd-port::output-port port::output-port)
      (open-input-zstd-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
      (open-output-zstd-file file-name::bstring
           #!optional (bufinfo #t))
      (file-zstd?::bbool file::bstring)))

;; make sure we initialize zstd as a part of module initialization
(cond-expand
   (bigloo-c
    ($bgl-zstd-init)))


(cond-expand
   (bigloo-c
    (define (input-port->zstd-port::input-port port::input-port #!optional (bufinfo #t))
       (let* ((stream ($bgl-zstd-create-decompress-stream))
              (buffer-size ($bgl-zstd-input-buffer-size))
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
                                          ($bgl-zstd-stream-decompress stream
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
                                                        (proc "zstd-input-proc")
                                                        (msg "unknown input state")
                                                        (obj res)))))))))
              (zstd-port (open-input-procedure proc bufinfo)))

          (input-port-close-hook-set! zstd-port
             (lambda (p) ($bgl-zstd-close-decompress-stream stream)
                     #unspecified))
          zstd-port)))
   (bigloo-jvm
    (define (input-port->zstd-port::input-port port::input-port #!optional (bufinfo #t))
       (let* ((input-port-stream ($input-port-stream-create port))
              (stream ($zstd-input-stream-create input-port-stream))
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
                                              (proc "zstd-input-proc")
                                              (msg "unknown input state")
                                              (obj bytes-read)))))))))
              (zstd-port (open-input-procedure proc bufinfo)))
        
          (input-port-close-hook-set! zstd-port
             (lambda (p) ($input-stream-close stream)
                #unspecified))
          zstd-port))))


(cond-expand
   (bigloo-c
    (define (output-port->zstd-port::output-port
               port::output-port)      
      (let* ((stream ($bgl-zstd-create-compress-stream 3 port))
             (writeproc (lambda (s)
                           ($bgl-zstd-stream-compress stream s (string-length s) #f)))
             (close (lambda ()
                       ($bgl-zstd-stream-compress stream "" 0 #t)
                       ($bgl-zstd-close-compress-stream stream)
                       #t))
             (zstd-port (open-output-procedure writeproc (lambda () #f) #t close)))       
         zstd-port)))
   
   (bigloo-jvm
    (define (output-port->zstd-port::output-port
               port::output-port)
       (let* ((output-port-stream ($output-port-stream-create port))
              (stream ($zstd-output-stream-create output-port-stream 3))
              (writeproc (lambda (s)
                            ($output-stream-write stream s)))
              (close (lambda ()
                        ($output-stream-close stream)
                        #t))
              (zstd-port (open-output-procedure writeproc
                            (lambda () #f) #t close)))
          zstd-port))))
              
(define (open-input-zstd-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))

   (let ((in (open-input-file file-name bufinfo timeout)))
      (if (input-port? in)
          (let ((input-port (input-port->zstd-port in)))
             (input-port-close-hook-set! input-port
                (lambda (p) (close-input-port in)))
             input-port))))


(define (open-output-zstd-file file-name::bstring
           #!optional (bufinfo #t))
   (let ((out (open-output-file file-name bufinfo)))
      (if (output-port? out)
          (let ((output-port (output-port->zstd-port out)))
             (output-port-close-hook-set! output-port
                (lambda (p) (close-output-port out)))
             output-port))))


(define (file-zstd?::bbool file::bstring)
   (with-handler (lambda (e)
                    #f)
                 (let ((input (open-input-zstd-file file)))
                    (if (input-port? input)
                        (unwind-protect
                           (begin (read-byte input)
                                  #t)
                           (close-input-port input))))))