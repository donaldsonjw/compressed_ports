(module compressed-ports/zlib-ports
   (extern
      (include "bgl_zlib.h")
      (type zlib-stream*
         (opaque)
         "struct zlib_stream *")
      (macro $bgl-zlib-create-inflate-stream::zlib-stream* (zlib-format::obj)
             "bgl_zlib_create_inflate_stream")
      (macro $bgl-zlib-create-deflate-stream::zlib-stream*
         (level::int zlib-format::obj) "bgl_zlib_create_deflate_stream")
      (macro $bgl-zlib-stream-inflate::obj (stream::zlib-stream*
                                              buffer::string length::long)
         "bgl_zlib_stream_inflate")
      (macro $bgl-zlib-stream-deflate::obj (stream::zlib-stream*
                                              buffer::string length::long
                                              finish?::bbool)
             "bgl_zlib_stream_deflate")
      (macro $bgl-zlib-init::void () "bgl_zlib_init")
      (macro $bgl-zlib-close-input-stream::obj (stream::zlib-stream*)
         "bgl_zlib_close_input_stream")
      (macro $bgl-zlib-close-output-stream::obj (stream::zlib-stream*)
         "bgl_zlib_close_output_stream"))

   (java
      (class $inflater
         (constructor create (::bool))
         "java.util.zip.Inflater")
      (class $input-stream
         (method read::long (::$input-stream ::string) "read")
         (method close::void (::$input-stream) "close")
         "java.io.InputStream")
      (class $inflater-input-stream::$input-stream
         (constructor create (::$input-stream ::$inflater))
         "java.util.zip.InflaterInputStream")
      (class $gzip-input-stream::$input-stream
         (constructor create (::$input-stream))
         "java.util.zip.GZIPInputStream")
      (class $input-port-stream::$input-stream
         (constructor create (::input-port))
         "bigloo.lib.compressed_ports.input_port_stream")

      (class $deflater
         (constructor create (::int ::bool))
         "java.util.zip.Deflater")
      (class $output-stream
         (method write::void (::$output-stream ::string)
            "write")
         (method close::void (::$output-stream) "close")
         "java.io.OutputStream")
      (class $deflater-output-stream::$output-stream
         (constructor create (::$output-stream ::$deflater))
         "java.util.zip.DeflaterOutputStream")
      (class $gzip-output-stream::$output-stream
         (constructor create (::$output-stream))
         "java.util.zip.GZIPOutputStream")
      (class $output-port-stream::$output-stream
         (constructor create (::output-port))
         "bigloo.lib.compressed_ports.output_port_stream"))
   
   (export 
           (input-port->zlib-port::input-port port::input-port
              #!optional (bufinfo #t))
           (input-port->gzip-port::input-port port::input-port
              #!optional (bufinfo #t))
           (input-port->inflate-port::input-port port::input-port
              #!optional (bufinfo #t))
           (output-port->zlib-port::output-port port::output-port)
           (output-port->gzip-port::output-port port::output-port)
           (output-port->deflate-port::output-port port::output-port)
           (open-input-gzip-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
           (open-input-inflate-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
           (open-input-zlib-file file-name::bstring
           #!optional (bufinfo #t) (timeout 1000000))
           (open-output-gzip-file file-name::bstring #!optional (bufinfo #t))
           (open-output-deflate-file file-name::bstring #!optional (bufinfo #t))
           (open-output-zlib-file file-name::bstring #!optional (bufinfo #t))))


(define +zlib-compressed-formats+ '(GZIP ZLIB DEFLATE))

(define (zlib-supported-format? v::symbol)
   (member v +zlib-compressed-formats+))



;; make sure we initialize zlib as a part of module initialization
(cond-expand
   (bigloo-c ($bgl-zlib-init)))


(define +chunk-size+ 32768)

(define +gzip-magic-number+ #x8b1f)

(define +deflate-compression+ 8)
(define +os-unknown+ 255)

(define (read-uint16le port)
   (let* ((b1 (read-byte port))
          (b2 (read-byte port)))
      (bit-or b1
         (bit-lsh b2 8))))

(define (read-uint32le::uint32 port)
   (let* ((b1 (fixnum->uint32 (read-byte port)))
          (b2 (fixnum->uint32 (read-byte port)))
          (b3 (fixnum->uint32 (read-byte port)))
          (b4 (fixnum->uint32 (read-byte port))))
      (bit-oru32 (bit-oru32 (bit-lshu32 b4 24)
                 (bit-lshu32 b3 16))
         (bit-oru32 (bit-lshu32 b2 8)
            b1))))

(define (skip-null-terminated-string port)
   (let loop ((byte (read-byte port)))
      (if (= byte 0)
          #f
          (loop (read-byte port)))))

(define (skip-bytes port n)
   (do ((i 0 (+ i 1)))
       ((= i n))
       (read-byte port)))

(define-inline (elong->byte v::elong)
   (fixnum->byte (elong->fixnum v)))

(define (write-uint32le port val::elong)
   (write-byte (elong->byte (bit-andelong val #exff)))
   (write-byte (elong->byte (bit-andelong (bit-rshelong val 8) #exff)))
   (write-byte (elong->byte (bit-andelong (bit-rshelong val 16) #exff)))
   (write-byte (elong->byte (bit-andelong (bit-rshelong val 24) #exff))))

(define (parse-gzip-header port)
   (let ((magic (read-uint16le port)))
      (print "magic is " +gzip-magic-number+)
      (print "read magic is " magic)
      (unless (= magic +gzip-magic-number+)
         (error "read-gzip-header" "invalid gzip magic number" magic))
  
      (let ((compression-method (read-byte port)))
         (unless (= compression-method +deflate-compression+)
            (error "read-gzip-header" "unsupported compression method" compression-method))
         
         (let* ((flags (read-byte port))
                (ftext (> (bit-and flags #x1) 0))
                (fhcrc  (> (bit-and flags #x2) 0))
                (fextra (> (bit-and flags #x4) 0))
                (fname (> (bit-and flags #x8) 0))
                (fcomment (> (bit-and flags #x10) 0))
                (mtime (read-uint32le port))
                (xflags (read-byte port))
                (os (read-byte port)))

             (when fextra
                (let* ((xtr-length (read-uint16le port)))
                   (skip-bytes port xtr-length)))
             
             (when fname
                (skip-null-terminated-string port))
             
             (when fcomment
                (skip-null-terminated-string port))

             (when fhcrc
                (read-uint16le port))))))

(define (write-gzip-header port)
   (write-byte (bit-and #xFF +gzip-magic-number+) port)
   (write-byte (bit-and #xFF (bit-rsh +gzip-magic-number+ 8)) port)
   (write-byte +deflate-compression+ port)
   (do ((i 0 (+ i 1)))
       ((= i 6))
       (write-byte 0 port))
   (write-byte +os-unknown+ port))


(define (crc-32-update b crc::elong)
   (let ((poly (crc-polynomial-le 'ieee-32))
         (len (crc-length 'ieee-32)))
      (crc-elong b crc poly len)))

(define (crc-32-finish crc::elong)
   (bit-andelong crc #exffffffff))

(define (crc-32-string str crc::elong)
   (let loop ((i 0)
              (c crc))
      (if (= i (string-length str))
          c
          (loop (+ i 1)
             (crc-32-update (string-ref str i) c)))))


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
             (lambda (p) ($bgl-zlib-close-input-stream stream)
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

(define (output-port->gzip-port::output-port port::output-port)
   (%output-port->zlib-port port 'GZIP))

(define (output-port->zlib-port::output-port port::output-port)
   (%output-port->zlib-port port 'ZLIB))

(define (output-port->deflate-port::output-port port::output-port)
   (%output-port->zlib-port port 'DEFLATE))


(cond-expand
   (bigloo-c
    (define (%output-port->zlib-port::output-port
               port::output-port zlib-format::symbol)
       (define (compress-and-write stream s flush)
          (let loop ((compressed-data ($bgl-zlib-stream-deflate stream
                                         s
                                         (string-length s)
                                         flush)))
             (cond ((string? compressed-data)
                    (display compressed-data port)
                    (loop ($bgl-zlib-stream-deflate stream "" 0 flush)))
                   ((eq? compressed-data 'DEFLATE-CONTINUE)
                    #unspecified))))
       
       (let* ((stream ($bgl-zlib-create-deflate-stream 9 zlib-format))
              (writeproc (lambda (s)
                            (compress-and-write stream s #f)))
              (close (lambda ()
                        (compress-and-write stream "" #t)
                        ($bgl-zlib-close-output-stream stream)
                        #t))
              (zlib-port (open-output-procedure writeproc (lambda () #f) #t close)))       
          zlib-port)))
   (bigloo-jvm
    (define (%output-port->zlib-port::output-port
               port::output-port zlib-format::symbol) 
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
              (zlib-port (open-output-procedure writeproc (lambda () #f) #t close)))
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
                (lambda () (close-input-port in)))
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
   (let ((out (open-output-file file-name)))
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
                (lambda () (close-output-port out)))
             output-port))))

(define (open-output-gzip-file file-name::bstring #!optional (bufinfo #t))
   (%open-output-zlib-file file-name 'GZIP bufinfo))

(define (open-output-deflate-file file-name::bstring #!optional (bufinfo #t))
   (%open-output-zlib-file file-name 'DEFLATE bufinfo))

(define (open-output-zlib-file file-name::bstring #!optional (bufinfo #t))
   (%open-output-zlib-file file-name 'ZLIB bufinfo))






