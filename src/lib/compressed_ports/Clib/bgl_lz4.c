#include "bigloo.h"
#include "bgl_lz4.h"
#include <lz4frame.h>
#include <lz4frame_static.h>

static obj_t DECOMPRESS_CONTINUE = BUNSPEC;
static obj_t DECOMPRESS_DONE = BUNSPEC;


struct lz4_compress_stream {
  LZ4F_compressionContext_t cctx;
  obj_t output;
  size_t buffer_length;
  unsigned char buffer[];
};


struct lz4_decompress_stream {
  LZ4F_decompressionContext_t dctx;
  size_t src_length;
  unsigned char* src_buffer;
  size_t dest_length;
  unsigned char dest_buffer[];
};

static const LZ4F_preferences_t kPrefs = {
    { 0, 0, 0, LZ4F_frame,
      0 /* unknown content size */, 0 /* no dictID */ , 0},
    0,   /* compression level; 0 == default */
    0,   /* autoflush */
    0,   /* favor decompression speed */
    { 0, 0, 0 },  /* reserved, must be set to 0 */
};

static obj_t EMPTY_STR = BUNSPEC;

BGL_RUNTIME_DEF void bgl_lz4_init() {
  static int initialized = 0;
  if(!initialized) {
    initialized = 1;
    DECOMPRESS_CONTINUE = string_to_symbol("DECOMPRESS-CONTINUE");
    DECOMPRESS_DONE = string_to_symbol("DECOMPRESS-DONE");
    EMPTY_STR = string_to_bstring("");
  }
}

BGL_RUNTIME_DEF lz4_compress_streamp
bgl_lz4_create_compress_stream(obj_t output) {
  unsigned long max_output_size = LZ4F_compressBound(LZ4_CHUNK_SIZE, &kPrefs);

  lz4_compress_streamp lz4stream
    = (lz4_compress_streamp) GC_MALLOC(sizeof(struct lz4_compress_stream)
                                        + max_output_size);
  lz4stream->output = output;
  lz4stream->buffer_length = max_output_size;
  LZ4F_errorCode_t ret = LZ4F_createCompressionContext(&(lz4stream->cctx),
                                                       LZ4F_VERSION);
  if (LZ4F_isError(ret)) {
    lz4stream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_ERROR,
                     "bgl_create_lz4_compress_stream",
                     (char*)LZ4F_getErrorName(ret),
                     BUNSPEC);
  }

  ret = LZ4F_compressBegin(lz4stream->cctx,
                           lz4stream->buffer,
                           lz4stream->buffer_length,
                           &kPrefs);
  if (LZ4F_isError(ret)) {
    lz4stream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_ERROR,
                     "bgl_create_lz4_compress_stream",
                     (char*)LZ4F_getErrorName(ret),
                     BUNSPEC);
  }

  // write header to output stream 
  bgl_write(lz4stream->output, lz4stream->buffer, ret);
  
  return lz4stream;
}

BGL_RUNTIME_DEF lz4_decompress_streamp
bgl_lz4_create_decompress_stream() {
  
  unsigned long output_size = LZ4F_compressBound(LZ4_CHUNK_SIZE, &kPrefs);
  lz4_decompress_streamp lz4stream
    = (lz4_decompress_streamp) GC_MALLOC(sizeof(struct lz4_decompress_stream)
                                        + output_size);
  lz4stream->src_buffer = NULL;
  lz4stream->src_length = 0;
  lz4stream->dest_length = output_size;
  LZ4F_errorCode_t ret = LZ4F_createDecompressionContext(&(lz4stream->dctx), LZ4F_VERSION);
   if (LZ4F_isError(ret)) {
    lz4stream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_ERROR,
                     "bgl_create_lz4_decompress_stream",
                     "failed to created lz4 ctx",
                     BUNSPEC);
  }
  return lz4stream;
}

BGL_RUNTIME_DEF obj_t
bgl_lz4_stream_compress(lz4_compress_streamp stream, unsigned char *buffer,
                        long length, obj_t finishp) {

  obj_t res = BNIL;
  size_t compressed_size = 0;

  if (finishp != BTRUE) {
      compressed_size = LZ4F_compressUpdate(stream->cctx,
                                                   stream->buffer,
                                                   stream->buffer_length,
                                                   buffer,
                                                   length,
                                                   NULL);
      
      if (LZ4F_isError(compressed_size)) {
        C_SYSTEM_FAILURE(BGL_IO_ERROR,
                         "bgl_lz4_stream_compress",
                         (char*)LZ4F_getErrorName(compressed_size),
                         BINT(compressed_size));
      }       
  } else {
    compressed_size = LZ4F_compressEnd(stream->cctx,
                                      stream->buffer,
                                      stream->buffer_length,
                                      NULL);
    
    if (LZ4F_isError(compressed_size)) {
      C_SYSTEM_FAILURE(BGL_IO_ERROR,
                       "bgl_lz4_stream_compress",
                       (char*)LZ4F_getErrorName(compressed_size),
                       BINT(compressed_size));
    }
  }

  bgl_write(stream->output, stream->buffer, compressed_size);
   
  return res;
}


BGL_RUNTIME_DEF obj_t
bgl_lz4_stream_decompress(lz4_decompress_streamp stream, unsigned char *buffer,
                          long length) {
  
  obj_t res = DECOMPRESS_CONTINUE;
  LZ4F_errorCode_t ret = 0;

  if (length > 0) {
    stream->src_length = length;
    stream->src_buffer = buffer;
  }

  /* maintain the original src and dest length so that we can
          calculate remaining src data and restore the dest buffer
          capacity. */
  size_t orig_src_length = stream->src_length;
  size_t orig_dest_length = stream->dest_length;
  
  ret = LZ4F_decompress(stream->dctx,
                        stream->dest_buffer,
                        &(stream->dest_length),
                        stream->src_buffer,
                        &(stream->src_length),
                        NULL);

  /* update src to point to remaining input data */
  stream->src_buffer = (unsigned char*)stream->src_buffer + stream->src_length;
  stream->src_length = orig_src_length - stream->src_length;
  
  if (LZ4F_isError(ret)) {
    C_SYSTEM_FAILURE(BGL_IO_ERROR,
                     "bgl_lz4_stream_decompress",
                     (char*)LZ4F_getErrorName(ret),
                     BINT(ret));
  }
  
  if (stream->dest_length > 0) {
    unsigned char* out = (unsigned char*)(stream->dest_buffer);
    res = string_to_bstring_len(out, stream->dest_length);
  }
    
  if (ret == 0 && stream->dest_length == 0) {
    res = DECOMPRESS_DONE;
  }

  stream->dest_length = orig_dest_length;
  return res;
}


BGL_RUNTIME_DEF obj_t
bgl_lz4_close_decompress_stream(lz4_decompress_streamp stream) {
  LZ4F_freeDecompressionContext(stream->dctx);
  return BUNSPEC;
}

BGL_RUNTIME_DEF obj_t
bgl_lz4_close_compress_stream(lz4_compress_streamp stream) {
  LZ4F_freeCompressionContext(stream->cctx);
  return BUNSPEC;
}
