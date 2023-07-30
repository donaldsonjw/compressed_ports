(module compressed-ports/java-port-interop
   (export (port-read-byte::obj port::input-port)
           (port-read-fill-string!::obj port::input-port buffer::string offset::long
              length::long))
   (java
      (class $input-stream
         (method read::long (::$input-stream ::string) "read")
         (method close::void (::$input-stream) "close")
         "java.io.InputStream")
      (class $input-port-stream::$input-stream
         (constructor create (::input-port))
         "bigloo.lib.compressed_ports.input_port_stream")
      (class $output-stream
         (method write::void (::$output-stream ::string)
            "write")
         (method close::void (::$output-stream) "close")
         "java.io.OutputStream")
      (class $output-port-stream::$output-stream
         (constructor create (::output-port))
         "bigloo.lib.compressed_ports.output_port_stream")
      
      (export port-read-byte "bgl_port_read_byte")
      (export port-read-fill-string! "bgl_port_read_fill_string")))

(define (port-read-byte::obj port::input-port)
   (read-byte port))

(define (port-read-fill-string!::obj port::input-port buffer::string offset::long
           length::long)
   (read-fill-string! buffer offset length port))

