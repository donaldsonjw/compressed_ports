#include "bigloo.h"
#include "bgl_xz.h"
#include <lzma.h>
#include <stdio.h>


static obj_t DECOMPRESS_CONTINUE = BUNSPEC;
static obj_t DECOMPRESS_DONE = BUNSPEC;


struct xz_compress_stream {
  lzma_stream strm;
  obj_t output;
  unsigned char buffer[BUFSIZ];
};


struct xz_decompress_stream {
  lzma_stream strm;
  unsigned char buffer[BUFSIZ]; 
};

BGL_RUNTIME_DEF void bgl_xz_init() {
  static int initialized = 0;
  if(!initialized) {
    initialized = 1;
    DECOMPRESS_CONTINUE = string_to_symbol("DECOMPRESS-CONTINUE");
    DECOMPRESS_DONE = string_to_symbol("DECOMPRESS-DONE");
  }
}

static void
  bgl_xz_error(unsigned char* proc, lzma_ret ret) {

  char *msg;
  switch (ret) {
  case LZMA_MEM_ERROR:
    msg = "Memory allocation failed";
    break;
       
  case LZMA_UNSUPPORTED_CHECK:
    msg = "The given check type is not supported by this liblzma build";
    break;

  case  LZMA_PROG_ERROR:
    msg = "One or more of the parameters have values that will never be valid.";
    break;

  case LZMA_BUF_ERROR:
    msg = "There is Not enough output buffer space.";
    break;

  case LZMA_DATA_ERROR:
    msg = "Invalid data detected.";
    break;

  case LZMA_OPTIONS_ERROR:
    msg = "Unsupported options provided.";
    break;
    
  default:
    msg = "Unknown error, possibly a bug";
    break;
  }

  C_SYSTEM_FAILURE(BGL_IO_ERROR,
                   proc, msg, BUNSPEC);
                   
}
  
BGL_RUNTIME_DEF xz_compress_streamp
bgl_xz_create_compress_stream(int preset, obj_t output) {

  xz_compress_streamp stream
    = (xz_compress_streamp) GC_MALLOC(sizeof(struct xz_compress_stream));

  stream->output = output;

  lzma_ret ret = lzma_easy_encoder(&(stream->strm), preset, LZMA_CHECK_CRC64);

  if (ret != LZMA_OK) {
    bgl_xz_error("bgl_xz_create_compress_stream", ret);
  }
  
  return stream;
}

BGL_RUNTIME_DEF xz_decompress_streamp
bgl_xz_create_decompress_stream() {
  xz_decompress_streamp stream
    = (xz_decompress_streamp) GC_MALLOC(sizeof(struct xz_decompress_stream));

  lzma_ret ret = lzma_stream_decoder(&(stream->strm), UINT64_MAX, LZMA_CONCATENATED);

  if (ret != LZMA_OK) {
     bgl_xz_error("bgl_xz_create_decompress_stream", ret);
  }

  return stream;
}

BGL_RUNTIME_DEF obj_t
bgl_xz_stream_compress(xz_compress_streamp stream, unsigned char *buffer,
                       long length, obj_t finishp) {

  obj_t res = BNIL; 

  int action = finishp == BTRUE ? LZMA_FINISH : LZMA_RUN;
  
  stream->strm.next_in = buffer;
  stream->strm.avail_in = length;

  do {
    stream->strm.avail_out = BUFSIZ;
    stream->strm.next_out = stream->buffer;
    
    lzma_ret ret = lzma_code(&(stream->strm), action);

    if (!(LZMA_OK == ret || LZMA_STREAM_END == ret)) {

      bgl_xz_error("bgl_xz_stream_compress", ret);

    }
    
    int have = BUFSIZ - stream->strm.avail_out;
    bgl_write(stream->output, stream->buffer, have);

  } while (stream->strm.avail_out == 0);
  
  return res;
}

BGL_RUNTIME_DEF obj_t
bgl_xz_stream_decompress(xz_decompress_streamp stream, unsigned char *buffer,
                         long length) {
  
  obj_t res = DECOMPRESS_CONTINUE;

  int available = 0;
  lzma_ret ret;
  
  if (length > 0) {
    stream->strm.next_in = buffer;
    stream->strm.avail_in = length;
  }
  
  stream->strm.avail_out = BUFSIZ;
  stream->strm.next_out = stream->buffer;
  ret = lzma_code(&(stream->strm), LZMA_RUN);
  
  if (!(LZMA_OK == ret || LZMA_STREAM_END == ret)) {
    bgl_xz_error("bgl_cx_stream_decompress", ret);
  }
  
  available = BUFSIZ - stream->strm.avail_out;
  if (available > 0) {
    unsigned char* out = (unsigned char*)(stream->buffer);
    res = string_to_bstring_len(out, available);
  } 
  
  if (available == 0 && ret == LZMA_STREAM_END) {
    res = DECOMPRESS_DONE;
  } 
  
  return res;
}

BGL_RUNTIME_DEF obj_t
bgl_xz_close_output_stream(xz_compress_streamp stream) {
  lzma_end(&(stream->strm));
  return BUNSPEC;
}

BGL_RUNTIME_DEF obj_t
bgl_xz_close_input_stream(xz_decompress_streamp stream) {
  lzma_end(&(stream->strm));
  return BUNSPEC;
}
