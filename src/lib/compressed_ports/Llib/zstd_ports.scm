(module compressed-ports/zstd-ports
   (extern
      (include "bgl_zstd.h")
      (type zstd-decompress-stream*
         (opaque)
         "struct zstd_decompress_stream *")
      (type zstd-compress-stream*
         (opaque)
         "struct zstd_compress_stream *")

      (macro $bgl-zstd-compress-input-remaining?::bbool (zstream::zstd-compress-stream*)
             "bgl_zstd_compress_input_remaining")
      
      (macro $bgl-zstd-input-buffer-size::long ()
             "bgl_zstd_input_buffer_size")

      (macro $bgl-zstd-output-buffer-size::long ()
             "bgl_zstd_output_buffer_size") 
      
      (macro $bgl-zstd-create-decompress-stream::zstd-decompress-stream* ()
             "bgl_zstd_create_decompress_stream")

      (macro $bgl-zstd-create-compress-stream::zstd-compress-stream* (level::int)
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

      (macro $bgl-zstd-close-output-stream::obj (zstream::zstd-compress-stream*)
             "bgl_zstd_close_output_stream")
      (macro $bgl-zstd-close-input-stream::obj (zstream::zstd-decompress-stream*)
             "bgl_zstd_close_input_stream"))

   (export
      (input-port->zstd-port::input-port port::input-port
         #!optional (bufinfo #t))
      (output-port->zstd-port::output-port port::output-port)
      (open-input-zstd-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
      (open-output-zstd-file file-name::bstring
           #!optional (bufinfo #t))))



;; make sure we initialize zstd as a part of module initialization
(cond-expand
   (bigloo-c
    ($bgl-zstd-init)))


(cond-expand
   (bigloo-c
    (define (input-port->zstd-port port::input-port #!optional (bufinfo #t))
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
             (lambda (p) ($bgl-zstd-close-input-stream stream)
                     #unspecified))
          zstd-port)))
   (bigloo-jvm
    (define (input-port->zstd-port port::input-port #!optional (bufinfo #t))
       (error "%input-port->zstd-port" "not implemented" #unspecified))))


(cond-expand
   (bigloo-c
    (define (output-port->zstd-port::output-port
               port::output-port)
      (define (compress-and-write stream s flush)
          (let loop ((compressed-data ($bgl-zstd-stream-compress stream
                                         s
                                         (string-length s)
                                         flush)))
             (cond ((string? compressed-data)
                    (display compressed-data port)
                    (if ($bgl-zstd-compress-input-remaining? stream)
                        (loop ($bgl-zstd-stream-compress stream "" 0 #f))))
                   ((eq? compressed-data 'COMPRESS-CONTINUE)
                    #unspecified))))
      
      (let* ((stream ($bgl-zstd-create-compress-stream 3))
             (writeproc (lambda (s)
                           (compress-and-write stream s #f)))
             (close (lambda ()
                       (compress-and-write stream "" #t)
                       ($bgl-zstd-close-output-stream stream)
                       #t))
             (zstd-port (open-output-procedure writeproc (lambda () #f) #t close)))       
         zstd-port))) 

       (bigloo-jvm
        (define (output-port->zstd-port::output-port
                   port::output-port)
           (error "output-port->zstd-port" "unimplemented for jvm" #unspecified))))
              
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


