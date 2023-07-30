#|
compressed_ports 

Copyright 2023, Joseph Donaldson

|#

 (module compressed_ports_make_lib
    (import compressed-ports/zlib-ports
            compressed-ports/zstd-ports
            compressed-ports/xz-ports
            compressed-ports/java-port-interop)
    (eval (export-all)))

