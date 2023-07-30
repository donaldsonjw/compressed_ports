#include "bigloo.h"
#include <zlib.h>
#include "bgl_zlib.h"

BGL_IMPORT obj_t string_append(obj_t, obj_t);

#define BGL_ZLIB_CHUNK_SIZE (1 << 15)

static obj_t INFLATE_CONTINUE = BUNSPEC;
static obj_t INFLATE_DONE = BUNSPEC;
static obj_t ZLIB = BUNSPEC;
static obj_t DEFLATE = BUNSPEC;
static obj_t GZIP = BUNSPEC;

struct zlib_stream {
  z_stream stream;
  // only used for output deflate ports 
  obj_t output;
  unsigned char buffer[BGL_ZLIB_CHUNK_SIZE];
};

BGL_RUNTIME_DEF void bgl_zlib_init() {
  static int initialized = 0;
  if (!initialized) {
    initialized = 1;
    INFLATE_DONE = string_to_symbol("INFLATE-DONE");
    INFLATE_CONTINUE = string_to_symbol("INFLATE-CONTINUE");
    ZLIB = string_to_symbol("ZLIB");
    DEFLATE = string_to_symbol("DEFLATE");
    GZIP = string_to_symbol("GZIP");
  }
}

BGL_RUNTIME_DEF zlib_streamp
bgl_zlib_create_inflate_stream(obj_t zlib_format) {
  zlib_streamp zstream = (zlib_streamp) GC_MALLOC(sizeof(struct zlib_stream));

 /* we indicate to zlib that we are dealing with a gzip wrapped data
    by adding 16 to the standard 15 bit window size. If it is raw
    inflate, we pass -window size. Yes, it is hackish but it is how
    zlib expects the user to indicate this fact. Otherwise, we set
    window_bits normally.
 */
  int window_bits;
  if (zlib_format == GZIP) {
    window_bits = (15 + 16);
  } else if (zlib_format == DEFLATE) {
    window_bits = -15;
  } else {
    window_bits = 15;
  }

  // only used for compression deflate output streams 
  zstream->output = BUNSPEC;
  
  zstream->stream.zalloc = Z_NULL; 
  zstream->stream.zfree = Z_NULL;
  zstream->stream.opaque = Z_NULL;
  zstream->stream.avail_in = 0;
  zstream->stream.next_in = Z_NULL;
  zstream->stream.avail_out = BGL_ZLIB_CHUNK_SIZE;
  zstream->stream.next_out = zstream->buffer;
  int res = inflateInit2(&(zstream->stream), window_bits);
  if (Z_OK != res) {
    zstream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_ERROR,
                     "bgl_create_zlib_inflate_stream",
                     zstream->stream.msg,
                     BINT(res));
  }
  
  return zstream;
}

BGL_RUNTIME_DEF zlib_streamp
bgl_zlib_create_deflate_stream(int level, obj_t zlib_format, obj_t output) {
  zlib_streamp zstream = (zlib_streamp) GC_MALLOC(sizeof(struct zlib_stream));

  /* we indicate to zlib that we are dealing with a gzip wrapped data
     by adding 16 to the standard 15 bit window size. If it is raw
     inflate, we pass -window size. Yes, it is hackish but it is how
     zlib expects the user to indicate this fact. Otherwise, we set
     window_bits normally.
  */
  int window_bits;
  if (zlib_format == GZIP) {
    window_bits = (15 + 16);
  } else if (zlib_format == DEFLATE) {
    window_bits = -15;
  } else {
    window_bits = 15;
  }

  zstream->output = output;
  
  zstream->stream.zalloc = Z_NULL;
  zstream->stream.zfree = Z_NULL;
  zstream->stream.opaque = Z_NULL;
  zstream->stream.avail_in = 0;
  zstream->stream.next_in = Z_NULL;
  zstream->stream.avail_out = BGL_ZLIB_CHUNK_SIZE;
  zstream->stream.next_out = zstream->buffer;
  int res = deflateInit2(&(zstream->stream), level, Z_DEFLATED,
                         window_bits, 8, Z_DEFAULT_STRATEGY);
  if (Z_OK != res) {
    zstream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_ERROR,
                       "bgl_create_zlib_deflate_stream",
                       zstream->stream.msg,
                     BINT(res));
  }
  
  return zstream;
}

BGL_RUNTIME_DEF obj_t
bgl_zlib_close_input_stream(zlib_streamp zstream) {
  inflateEnd(&(zstream->stream));
  return BUNSPEC;
}


BGL_RUNTIME_DEF obj_t
bgl_zlib_close_output_stream(zlib_streamp zstream) {
  deflateEnd(&(zstream->stream));
  return BUNSPEC;
}


BGL_RUNTIME_DEF obj_t
bgl_zlib_stream_inflate(zlib_streamp zstream, unsigned char* buffer, long length) {
  obj_t res = INFLATE_CONTINUE;
  int available = 0;
  int ret;
  
  if (length > 0) {
    zstream->stream.next_in = buffer;
    zstream->stream.avail_in = length;
  }
  
  zstream->stream.avail_out = BGL_ZLIB_CHUNK_SIZE;
  zstream->stream.next_out = zstream->buffer;
  ret = inflate(&(zstream->stream), Z_NO_FLUSH);
  
  if (Z_BUF_ERROR == ret) {
    return INFLATE_CONTINUE;
  }
  
  if (!(Z_OK == ret || Z_STREAM_END == ret)) {
    C_SYSTEM_FAILURE(BGL_IO_READ_ERROR,
                     "bgl_zlib_stream_inflate",
                     zstream->stream.msg,
                     BINT(ret));
  }
  
  available = BGL_ZLIB_CHUNK_SIZE - zstream->stream.avail_out;
  if (available > 0) {
    unsigned char* out = (unsigned char*)(zstream->buffer);
    res = string_to_bstring_len(out, available);
  } 
  
  if (available == 0 && ret == Z_STREAM_END) {
    res = INFLATE_DONE;
  } 
  
  return res;
}

BGL_RUNTIME_DEF obj_t
bgl_zlib_stream_deflate(zlib_streamp zstream, unsigned char* buffer, long length,
                        obj_t finishp) {
  obj_t res = BNIL; 

  int flush = finishp == BTRUE ? Z_FINISH : Z_NO_FLUSH;
  
  zstream->stream.next_in = buffer;
  zstream->stream.avail_in = length;

  do {
    zstream->stream.avail_out = BGL_ZLIB_CHUNK_SIZE;
    zstream->stream.next_out = zstream->buffer;
    
    int ret = deflate(&(zstream->stream), flush);

    if (!(Z_OK == ret || Z_STREAM_END == ret)) {
      C_SYSTEM_FAILURE(BGL_IO_WRITE_ERROR,
                       "bgl_zlib_stream_deflate",
                       zstream->stream.msg,
                       BINT(ret));
    }
    
    int have = BGL_ZLIB_CHUNK_SIZE - zstream->stream.avail_out;
    bgl_write(zstream->output, zstream->buffer, have);

  } while (zstream->stream.avail_out == 0);
  
  return res;
}

