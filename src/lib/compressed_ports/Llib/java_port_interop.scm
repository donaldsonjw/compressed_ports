(module compressed-ports/java-port-interop
   (export (port-read-byte::obj port::input-port)
           (port-write-byte::obj byte::ubyte port::output-port)
           (port-read-fill-string!::obj port::input-port buffer::string offset::long
              length::long))
   (java
      (export port-read-byte "bgl_port_read_byte")
      (export port-read-fill-string! "bgl_port_read_fill_string")
      (export port-write-byte "bgl_port_write_byte")))



(define (port-read-byte::obj port::input-port)
   (read-byte port))

(define (port-read-fill-string!::obj port::input-port buffer::string offset::long
           length::long)
   (read-fill-string! buffer offset length port))


(define (port-write-byte byte::ubyte port::output-port)
   (write-byte byte port))