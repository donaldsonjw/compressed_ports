package bigloo.lib.compressed_ports;

import java.io.InputStream;
import bigloo.input_port;
import bigloo.obj;
import bigloo.bint;
import bigloo.foreign;
import bigloo.lib.compressed_ports.Llib.java_port_interop;

public class input_port_stream extends InputStream {
    
    private final input_port input;


    public input_port_stream(input_port input) {
        super();
        this.input = input;
    }

    @Override
    public int read () {
        obj res = 
            (obj)java_port_interop.bgl_port_read_byte(input);
        if (foreign.EOF_OBJECTP(res)) {
            return -1;
        } else {
            return foreign.BINT_TO_INT32((bint)res);
        }
    }

    @Override
    public int read(byte[] buffer) {
        return read(buffer, 0, buffer.length);
    }

    @Override
    public int read(byte[] buffer, int offset, int length) {
        obj res
            =  (obj)java_port_interop.bgl_port_read_fill_string(input, buffer, offset, length);
        if (foreign.EOF_OBJECTP(res)) {
            return -1;
        } else {
            return foreign.BINT_TO_INT32((bint)res);
        }
    }

    
}
