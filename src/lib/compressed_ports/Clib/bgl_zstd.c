#include "bigloo.h"
#include "bgl_zstd.h"
#include <zstd.h>


static obj_t DECOMPRESS_CONTINUE = BUNSPEC;
static obj_t DECOMPRESS_DONE = BUNSPEC;


struct zstd_decompress_stream {
  ZSTD_DCtx* dctx;
  ZSTD_inBuffer input;
  long length;
  unsigned char buffer[];
};

struct zstd_compress_stream {
  ZSTD_CCtx* cctx;
  ZSTD_inBuffer input;
  obj_t output;
  long length;
  unsigned char buffer[];

};



BGL_RUNTIME_DEF void bgl_zstd_init() {
  static int initialized = 0;
  if(!initialized) {
    initialized = 1;
    DECOMPRESS_CONTINUE = string_to_symbol("DECOMPRESS-CONTINUE");
    DECOMPRESS_DONE = string_to_symbol("DECOMPRESS-DONE");
  }
}

BGL_RUNTIME_DEF long bgl_zstd_input_buffer_size() {
  return ZSTD_DStreamInSize();
}

BGL_RUNTIME_DEF long bgl_zstd_output_buffer_size() {
  return ZSTD_CStreamInSize();
}


BGL_RUNTIME_DEF zstd_decompress_streamp
bgl_zstd_create_decompress_stream() {
  zstd_decompress_streamp zstream = (zstd_decompress_streamp) GC_MALLOC(sizeof(struct zstd_decompress_stream) + ZSTD_DStreamOutSize());
  zstream->length = ZSTD_DStreamOutSize();
  zstream->dctx = ZSTD_createDCtx();
  if (NULL == zstream->dctx) {
    zstream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_ERROR,
                     "bgl_create_zstd_decompress_stream",
                     "failed to created zstd dctx",
                     BUNSPEC);
  }
  return zstream;
}

BGL_RUNTIME_DEF zstd_compress_streamp
bgl_zstd_create_compress_stream(int level, obj_t output) {
  zstd_compress_streamp zstream = (zstd_compress_streamp) GC_MALLOC(sizeof(struct zstd_compress_stream) + ZSTD_CStreamOutSize());
  zstream->output = output;
  zstream->length = ZSTD_CStreamOutSize();
  zstream->cctx = ZSTD_createCCtx();
  if (NULL == zstream->cctx) {
    zstream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_ERROR,
                     "bgl_create_zstd_decompress_stream",
                     "failed to created zstd dctx",
                     BUNSPEC);
  }

  size_t ret;
  ret = ZSTD_CCtx_setParameter(zstream->cctx, ZSTD_c_compressionLevel, level);
  if (ZSTD_isError(ret)) {
    zstream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_READ_ERROR,
                     "bgl_zstd_create compress_stream",
                     (char*)ZSTD_getErrorName(ret),
                     ELONG_TO_BELONG(ret));
  }
  
  ret = ZSTD_CCtx_setParameter(zstream->cctx, ZSTD_c_checksumFlag, 1);
  if (ZSTD_isError(ret)) {
    zstream = NULL;
    C_SYSTEM_FAILURE(BGL_IO_READ_ERROR,
                     "bgl_zstd_create compress_stream",
                     (char*)ZSTD_getErrorName(ret),
                     ELONG_TO_BELONG(ret));
  }
  
  return zstream;
}

BGL_RUNTIME_DEF obj_t
bgl_zstd_stream_compress(zstd_compress_streamp zstream, unsigned char* buffer, long length, obj_t finishp) {
  ZSTD_EndDirective const mode = finishp == BTRUE ? ZSTD_e_end : ZSTD_e_continue;

  zstream->input.src = buffer;
  zstream->input.pos = 0;
  zstream->input.size = length;
  
  int finished;
  do {
    
    ZSTD_outBuffer output = { zstream->buffer, zstream->length, 0 };

    size_t remaining = ZSTD_compressStream2(zstream->cctx, &output , &(zstream->input), mode);
    if (ZSTD_isError(remaining)) {
      C_SYSTEM_FAILURE(BGL_IO_READ_ERROR,
                       "bgl_zstd_stream_compress",
                       (char*)ZSTD_getErrorName(remaining),
                       ELONG_TO_BELONG(remaining));   
    }

    bgl_write(zstream->output, zstream->buffer, output.pos);

    finished = finishp == BTRUE ? (remaining == 0)
      : (zstream->input.pos == zstream->input.size);
  } while (!finished);
  

  return BUNSPEC;
}

BGL_RUNTIME_DEF obj_t
bgl_zstd_stream_decompress(zstd_decompress_streamp zstream, unsigned char* buffer, long length) {
  obj_t res = DECOMPRESS_CONTINUE;

  if (length > 0) {
    zstream->input.size = length;
    zstream->input.pos = 0;
    zstream->input.src = buffer;
  }
    
  
  ZSTD_outBuffer output = {zstream->buffer, zstream->length, 0};

  size_t ret = ZSTD_decompressStream(zstream->dctx, &output, &(zstream->input));
  if (ZSTD_isError(ret)) {
    C_SYSTEM_FAILURE(BGL_IO_READ_ERROR,
                     "bgl_zstd_stream_decompress",
                     (char*)ZSTD_getErrorName(ret),
                     ELONG_TO_BELONG(ret));
  }

  if (output.pos > 0) {
    res = string_to_bstring_len(zstream->buffer, output.pos);
  }

  if (output.pos == 0 && zstream->input.pos > zstream->input.size) {
    res = DECOMPRESS_CONTINUE;
  }
  
  return res;
}

BGL_RUNTIME_DEF obj_t
bgl_zstd_close_decompress_stream(zstd_decompress_streamp zstream) {
      ZSTD_freeDCtx(zstream->dctx);
      return BUNSPEC;
}

BGL_RUNTIME_DEF obj_t
bgl_zstd_close_compress_stream(zstd_compress_streamp zstream) {
      ZSTD_freeCCtx(zstream->cctx);
      return BUNSPEC;
}
