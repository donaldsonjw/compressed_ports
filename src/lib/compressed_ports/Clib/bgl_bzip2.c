#include "bigloo.h"
#include "bgl_bzip2.h"
#include <bzlib.h>

#define BZ2_BUFFER_SIZE (1 << 15)

#define NOT_VERBOSE 0
#define DEFAULT_WORKFACTOR 0
#define NOT_SMALL 0

static obj_t DECOMPRESS_CONTINUE = BUNSPEC;
static obj_t DECOMPRESS_DONE = BUNSPEC;



struct bzip2_compress_stream {
  bz_stream strm;
  obj_t output;
  unsigned char buffer[BZ2_BUFFER_SIZE];
};


struct bzip2_decompress_stream {
  bz_stream strm;
  int ret;
  unsigned char buffer[BZ2_BUFFER_SIZE];
};



BGL_RUNTIME_DEF void bgl_bzip2_init() {
  static int initialized = 0;
  if(!initialized) {
    initialized = 1;
    DECOMPRESS_CONTINUE = string_to_symbol("DECOMPRESS-CONTINUE");
    DECOMPRESS_DONE = string_to_symbol("DECOMPRESS-DONE");
  }
}

static void bgl_bzip2_error(unsigned char* proc, int ret) {
  char* msg;
  switch (ret) {
  case BZ_CONFIG_ERROR:
    msg = "The bzip2 library has been miscompiled.";
    break;
  case BZ_PARAM_ERROR:
    msg = "One or more parameters are invalid.";
    break;
  case BZ_MEM_ERROR:
    msg = "bzip2 was unable to allocate the necessary memory.";
    break;
  case BZ_DATA_ERROR:
    msg = "A data integrity error has been detected.";
    break;
  case BZ_DATA_ERROR_MAGIC:
    msg = "The compressed data stream did not start with the expected magic bytes.";
    break;
  default:
    msg = "Unkown error, possibly a bug.";
  }
  
  C_SYSTEM_FAILURE(BGL_IO_ERROR, proc, msg, BINT(ret));
}


BGL_RUNTIME_DEF bzip2_compress_streamp
bgl_bzip2_create_compress_stream(int block_size, obj_t output) {
  bzip2_compress_streamp stream
    = (bzip2_compress_streamp) GC_MALLOC(sizeof(struct bzip2_compress_stream));

  stream->output = output;
  
  int ret =  BZ2_bzCompressInit(&(stream->strm),
                                block_size,
                                NOT_VERBOSE,
                                DEFAULT_WORKFACTOR);
  if (ret != BZ_OK) {
    bgl_bzip2_error("bgl_bzip2_create_compress_stream", ret);
  }

  return stream;
}


BGL_RUNTIME_DEF bzip2_decompress_streamp
bgl_bzip2_create_decompress_stream () {
  bzip2_decompress_streamp stream
    = (bzip2_decompress_streamp) GC_MALLOC(sizeof(struct bzip2_decompress_stream));

  stream->strm.bzalloc = NULL;
  stream->strm.bzfree = NULL;
  stream->ret = BZ_OK;
  int ret =
    BZ2_bzDecompressInit(&(stream->strm), NOT_VERBOSE, NOT_SMALL);
  if ( ret != BZ_OK) {
    bgl_bzip2_error("bgl_bzip2_create_decompress_stream", ret);
  }

  return stream;
}

BGL_RUNTIME_DEF obj_t
bgl_bzip2_stream_compress(bzip2_compress_streamp stream,
                         unsigned char* buffer,
                         long length, obj_t finishp) {
  obj_t res = BNIL;

  int action = finishp == BTRUE ? BZ_FINISH : BZ_RUN;

  stream->strm.next_in = buffer;
  stream->strm.avail_in = length;

  do {
    stream->strm.avail_out = BZ2_BUFFER_SIZE;
    stream->strm.next_out = stream->buffer;
    
    int ret = BZ2_bzCompress(&(stream->strm), action);

    if (!(BZ_RUN_OK == ret || BZ_STREAM_END == ret)) {

      bgl_bzip2_error("bgl_bzip2_stream_compress", ret);

    }
    
    int have = BZ2_BUFFER_SIZE - stream->strm.avail_out;
    bgl_write(stream->output, stream->buffer, have);

  } while (stream->strm.avail_out == 0);
  
  return res;
}

BGL_RUNTIME_DEF obj_t
bgl_bzip2_stream_decompress(bzip2_decompress_streamp stream,
                            unsigned char *buffer,
                            long length) {
  
  obj_t res = DECOMPRESS_CONTINUE;

  int available = 0;

  if (length == 0 && stream->ret == BZ_STREAM_END) {
    return DECOMPRESS_DONE;
  }
  
  if (length > 0) {
    stream->strm.next_in = buffer;
    stream->strm.avail_in = length;
  }
  
  stream->strm.avail_out = BZ2_BUFFER_SIZE;
  stream->strm.next_out = stream->buffer;
  
  stream->ret = BZ2_bzDecompress(&(stream->strm));
  
  if (!(BZ_OK == stream->ret || BZ_STREAM_END == stream->ret)) {
    bgl_bzip2_error("bgl_bzip2_stream_decompress", stream->ret);
  }
  
  available = BZ2_BUFFER_SIZE - stream->strm.avail_out;
  if (available > 0) {
    unsigned char* out = (unsigned char*)(stream->buffer);
    res = string_to_bstring_len(out, available);
  } 
    
  return res;
}

BGL_RUNTIME_DEF obj_t
bgl_bzip2_close_compress_stream(bzip2_compress_streamp stream) {
  BZ2_bzCompressEnd(&(stream->strm));
  return BUNSPEC;
}

BGL_RUNTIME_DEF obj_t
bgl_bzip2_close_decompress_stream(bzip2_decompress_streamp stream) {
  BZ2_bzDecompressEnd(&(stream->strm));
  return BUNSPEC;
}
