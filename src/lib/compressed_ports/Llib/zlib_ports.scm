(module compressed-ports/zlib-ports
   (import compressed-ports/java-port-interop)
   (extern
      (include "bgl_zlib.h")
      (type zlib-stream*
         (opaque)
         "struct zlib_stream *")
      (macro $bgl-zlib-create-inflate-stream::zlib-stream* (zlib-format::obj)
             "bgl_zlib_create_inflate_stream")
      (macro $bgl-zlib-create-deflate-stream::zlib-stream*
         (level::int zlib-format::obj output::output-port) "bgl_zlib_create_deflate_stream")
      (macro $bgl-zlib-stream-inflate::obj (stream::zlib-stream*
                                              buffer::string length::long)
         "bgl_zlib_stream_inflate")
      (macro $bgl-zlib-stream-deflate::obj (stream::zlib-stream*
                                              buffer::string length::long
                                              finish?::bbool)
             "bgl_zlib_stream_deflate")
      (macro $bgl-zlib-init::void () "bgl_zlib_init")
      (macro $bgl-zlib-close-decompress-stream::obj (stream::zlib-stream*)
         "bgl_zlib_close_decompress_stream")
      (macro $bgl-zlib-close-compress-stream::obj (stream::zlib-stream*)
         "bgl_zlib_close_compress_stream"))

   (java
      (class $inflater
         (constructor create (::bool))
         "java.util.zip.Inflater")
      (class $inflater-input-stream::$input-stream
         (constructor create (::$input-stream ::$inflater))
         "java.util.zip.InflaterInputStream")
      (class $gzip-input-stream::$input-stream
         (constructor create (::$input-stream))
         "java.util.zip.GZIPInputStream")
      (class $deflater
         (constructor create (::int ::bool))
         "java.util.zip.Deflater")
      (class $deflater-output-stream::$output-stream
         (constructor create (::$output-stream ::$deflater))
         "java.util.zip.DeflaterOutputStream")
      (class $gzip-output-stream::$output-stream
         (constructor create (::$output-stream))
         "java.util.zip.GZIPOutputStream"))
   
   (export 
      (input-port->zlib-port::input-port port::input-port
         #!optional (bufinfo #t))
      (input-port->gzip-port::input-port port::input-port
         #!optional (bufinfo #t))
      (input-port->inflate-port::input-port port::input-port
         #!optional (bufinfo #t))
      (output-port->zlib-port::output-port port::output-port #!optional (bufinfo #t))
      (output-port->gzip-port::output-port port::output-port #!optional (bufinfo #t))
      (output-port->deflate-port::output-port port::output-port  #!optional (bufinfo #t))
      (open-input-gzip-file file-name::bstring
         #!optional (bufinfo #t) (timeout 1000000))
      (open-input-inflate-file file-name::bstring
         #!optional (bufinfo #t) (timeout 1000000))
      (open-input-zlib-file file-name::bstring
         #!optional (bufinfo #t) (timeout 1000000))
      (open-output-gzip-file file-name::bstring #!optional (bufinfo #t))
      (open-output-deflate-file file-name::bstring #!optional (bufinfo #t))
      (open-output-zlib-file file-name::bstring #!optional (bufinfo #t))
      (file-gzip?::bbool file::bstring)
      (file-deflate?::bbool file::bstring)
      (file-zlib?::bbool file::bstring)
      (open-input-gzip-port in::input-port #!optional (bufinfo #t))
      (open-input-zlib-port in::input-port #!optional (bufinfo #t))
      (open-input-inflate-port in::input-port #!optional (bufinfo #t))
      (open-output-gzip-port out::output-port #!optional (bufinfo #t))
      (open-output-zlib-port out::output-port #!optional (bufinfo #t))
      (open-output-deflate-port out::output-port #!optional (bufinfo #t))))


(define +zlib-compressed-formats+ '(GZIP ZLIB DEFLATE))

(define (zlib-supported-format? v::symbol)
   (member v +zlib-compressed-formats+))



;; make sure we initialize zlib as a part of module initialization
(cond-expand
   (bigloo-c ($bgl-zlib-init)))


(define +chunk-size+ 32768)

(cond-expand
   (bigloo-c
    (define (%input-port->zlib-port port::input-port zlib-format::symbol
               #!optional (bufinfo #t))
       (let* ((stream ($bgl-zlib-create-inflate-stream zlib-format))
              (buffer (make-string +chunk-size+))
              (need-compressed-data? #t)
              (proc (lambda ()
                       (let loop ()
                          (let* ((bytes-read (if need-compressed-data?
                                                 (let ((br (read-chars! buffer +chunk-size+ port)))
                                                    (set! need-compressed-data? #f)
                                                    br)
                                                 0))
                                 (res ($bgl-zlib-stream-inflate stream
                                         (if (> bytes-read 0)
                                             buffer "")
                                         bytes-read)))
                             
                             (cond ((string? res)                 
                                    res)
                                   ((eq? res 'INFLATE-CONTINUE)
                                    (set! need-compressed-data? #t)
                                    (loop))
                                   ((eq? res 'INFLATE-DONE)
                                              #f)
                                   (else
                                    (raise (instantiate::&io-error
                                                        (proc "gzip-input-proc")
                                                        (msg "unknown input state")
                                                        (obj res)))))))))
              (zlib-port (open-input-procedure proc bufinfo)))
          
          (input-port-close-hook-set! zlib-port
             (lambda (p) ($bgl-zlib-close-decompress-stream stream)
                #unspecified))
          zlib-port)))
   (bigloo-jvm
    (define (%input-port->zlib-port port::input-port zlib-format::symbol
               #!optional (bufinfo #t))
       
       (let* ((input-port-stream ($input-port-stream-create port))
              (stream (if (eq? zlib-format 'GZIP) 
                          ($gzip-input-stream-create input-port-stream)
                          ($inflater-input-stream-create input-port-stream
                             ($inflater-create (eq? zlib-format 'DEFLATE)))))
              (buffer (make-string +chunk-size+))
              (proc (lambda ()
                       (let loop ()
                          (let* ((bytes-read ($input-stream-read stream buffer)))
                             
                             (cond ((> bytes-read 0)                 
                                    (string-shrink! buffer bytes-read))
                                   ((= -1 bytes-read)
                                    #f)
                                   (else
                                    (raise (instantiate::&io-error
                                              (proc "gzip-input-proc")
                                              (msg "unknown input state")
                                              (obj bytes-read)))))))))
              (zlib-port (open-input-procedure proc bufinfo)))
        
          (input-port-close-hook-set! zlib-port
             (lambda (p) ($input-stream-close stream)
                #unspecified))
          zlib-port))))




(define (input-port->gzip-port::input-port port::input-port #!optional (bufinfo #t))
   (%input-port->zlib-port port 'GZIP bufinfo))

(define (input-port->zlib-port::input-port port::input-port #!optional (bufinfo #t))
   (%input-port->zlib-port port 'ZLIB bufinfo))

(define (input-port->inflate-port::input-port port::input-port
           #!optional (bufinfo #t))
   (%input-port->zlib-port port 'DEFLATE bufinfo))

(define (output-port->gzip-port::output-port port::output-port #!optional (bufinfo #t))
   (%output-port->zlib-port port 'GZIP bufinfo))

(define (output-port->zlib-port::output-port port::output-port #!optional (bufinfo #t))
   (%output-port->zlib-port port 'ZLIB bufinfo))

(define (output-port->deflate-port::output-port port::output-port  #!optional (bufinfo #t))
   (%output-port->zlib-port port 'DEFLATE bufinfo))


(cond-expand
   (bigloo-c
    (define (%output-port->zlib-port::output-port
               port::output-port zlib-format::symbol #!optional (bufinfo #t))  
       (let* ((stream ($bgl-zlib-create-deflate-stream 9 zlib-format port))
              (writeproc (lambda (s)
                            ($bgl-zlib-stream-deflate stream s (string-length s)
                               #f)))
              (close (lambda ()
                        ($bgl-zlib-stream-deflate stream "" 0 #t)
                        ($bgl-zlib-close-compress-stream stream)
                        #t))
              (zlib-port (open-output-procedure writeproc (lambda () #f) bufinfo close)))       
          zlib-port)))
   (bigloo-jvm
    (define (%output-port->zlib-port::output-port
               port::output-port zlib-format::symbol #!optional (bufinfo #t)) 
       (let* ((output-port-stream ($output-port-stream-create port))
              (stream (if (eq? zlib-format 'GZIP) 
                          ($gzip-output-stream-create output-port-stream)
                          ($deflater-output-stream-create output-port-stream
                             ($deflater-create -1 (eq? zlib-format 'DEFLATE)))))
              (writeproc (lambda (s)
                            ($output-stream-write stream s)))
              (close (lambda ()
                        ($output-stream-close stream)
                        #t))
              (zlib-port (open-output-procedure writeproc (lambda () #f) bufinfo close)))
          zlib-port))))



(define (%open-input-zlib-file file-name::bstring zlib-format::symbol
           #!optional (bufinfo #t) (timeout 1000000))
   (let ((in (open-input-file file-name bufinfo timeout)))
      (if (input-port? in)
          (let ((input-port (case zlib-format 
                               ((GZIP)
                                (input-port->gzip-port in))
                               ((ZLIB)
                                (input-port->zlib-port in))
                               ((DEFLATE)
                                (input-port->inflate-port in))
                               (else
                                (error "%open-input-zlib-file"
                                   "unsupported input format" zlib-format)))))
             (input-port-close-hook-set! input-port
                (lambda (p) (close-input-port in)))
             input-port))))


(define (open-input-gzip-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
   (%open-input-zlib-file file-name 'GZIP bufinfo timeout))

(define (open-input-inflate-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
   (%open-input-zlib-file file-name 'DEFLATE bufinfo timeout))

(define (open-input-zlib-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
   (%open-input-zlib-file file-name 'ZLIB bufinfo timeout))

(define (%open-output-zlib-file file-name::bstring zlib-format::symbol 
           #!optional (bufinfo #t))
   (let ((out::output-port (open-output-file file-name bufinfo)))
      (if (output-port? out)
          (let ((output-port (case zlib-format 
                               ((GZIP)
                                (output-port->gzip-port out))
                               ((ZLIB)
                                (output-port->zlib-port out))
                               ((DEFLATE)
                                (output-port->deflate-port out))
                               (else
                                (error "%open-output-zlib-file"
                                   "unsupported input format" zlib-format)))))
             (output-port-close-hook-set! output-port
                (lambda (p) (close-output-port out)))
             output-port))))

(define (open-output-gzip-file file-name::bstring #!optional (bufinfo #t))
   (%open-output-zlib-file file-name 'GZIP bufinfo))

(define (open-output-deflate-file file-name::bstring #!optional (bufinfo #t))
   (%open-output-zlib-file file-name 'DEFLATE bufinfo))

(define (open-output-zlib-file file-name::bstring #!optional (bufinfo #t))
   (%open-output-zlib-file file-name 'ZLIB bufinfo))


(define (file-gzip?::bbool file::bstring)
   (with-handler (lambda (e)
                    #f)
                 (let ((input (open-input-gzip-file file)))
                    (if (input-port? input)
                        (unwind-protect
                           (begin (read-byte input)
                                  #t)
                           (close-input-port input))))))

(define (file-deflate?::bbool file::bstring)
   (with-handler (lambda (e)
                    #f)
                 (let ((input (open-input-inflate-file file)))
                    (if (input-port? input)
                        (unwind-protect
                           (begin (read-byte input)
                                  #t)
                           (close-input-port input))))))

(define (file-zlib?::bbool file::bstring)
   (with-handler (lambda (e)
                    #f)
                 (let ((input (open-input-zlib-file file)))
                    (if (input-port? input)
                        (unwind-protect
                           (begin (read-byte input)
                                  #t)
                           (close-input-port input))))))

(define (open-input-gzip-port in::input-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-input-gzip-port" bufinfo c-default-io-bufsiz)))
      (input-port->gzip-port in buf)))

(define (open-input-zlib-port in::input-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-input-zlib-port" bufinfo c-default-io-bufsiz)))
      (input-port->zlib-port in buf)))

(define (open-input-inflate-port in::input-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-input-inflate-port" bufinfo c-default-io-bufsiz)))
      (input-port->inflate-port in buf)))

(define (open-output-gzip-port out::output-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-output-gzip-port" bufinfo c-default-io-bufsiz)))
      (output-port->gzip-port out buf)))

(define (open-output-zlib-port out::output-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-output-zlib-port" bufinfo c-default-io-bufsiz)))
      (output-port->zlib-port out buf)))

(define (open-output-deflate-port out::output-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-output-deflate-port" bufinfo c-default-io-bufsiz)))
      (output-port->deflate-port out buf)))



