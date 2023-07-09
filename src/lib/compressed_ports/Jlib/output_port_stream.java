package bigloo.lib.compressed_ports;

import java.io.OutputStream;
import bigloo.output_port;
import bigloo.foreign;
import bigloo.lib.compressed_ports.Llib.java_port_interop;

public class output_port_stream extends OutputStream {
    private final output_port output;
    
    public output_port_stream(output_port output) {
        super();
        this.output = output;
    }
    
    @Override
    public void write(int b) {
        java_port_interop.bgl_port_write_byte(foreign.BINT_TO_UBYTE(foreign.BINT(b)),
                                              output);
    }   
}
