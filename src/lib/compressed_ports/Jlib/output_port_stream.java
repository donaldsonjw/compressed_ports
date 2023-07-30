package bigloo.lib.compressed_ports;

import java.io.OutputStream;
import bigloo.output_port;
import bigloo.foreign;

public class output_port_stream extends OutputStream {
    private final output_port output;
    
    public output_port_stream(output_port output) {
        super();
        this.output = output;
    }
    
    @Override
    public void write(int b) {
        output.write(b);
    }

    @Override
    public void write(byte[] b) {
        output.write(b);
    }

    @Override
    public void write(byte[] b, int off, int len) {
        output.write(b, off, len);
    }
}
