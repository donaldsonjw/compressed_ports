#include "bigloo.h"
#include "bgl_zstd.h"
#include <zstd.h>


static obj_t DECOMPRESS_CONTINUE = BUNSPEC;
static obj_t DECOMPRESS_DONE = BUNSPEC;
static obj_t COMPRESS_CONTINUE = BUNSPEC;
static obj_t COMPRESS_DONE = BUNSPEC;


struct zstd_decompress_stream {
  ZSTD_DCtx* dctx;
  ZSTD_inBuffer input;
  int length;
  unsigned char buffer[];
};

struct zstd_compress_stream {
  ZSTD_CCtx* cctx;
  ZSTD_inBuffer input;
  int length;
  unsigned char buffer[];
};



BGL_RUNTIME_DEF void bgl_zstd_init() {
  static int initialized = 0;
  if(!initialized) {
    initialized = 1;
    DECOMPRESS_CONTINUE = string_to_symbol("DECOMPRESS-CONTINUE");
    DECOMPRESS_DONE = string_to_symbol("DECOMPRESS-DONE");
    COMPRESS_CONTINUE = string_to_symbol("COMPRESS-CONTINUE");
    COMPRESS_DONE = string_to_symbol("COMPRESS-DONE");
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
bgl_zstd_create_compress_stream(int level) {
  zstd_compress_streamp zstream = (zstd_compress_streamp) GC_MALLOC(sizeof(struct zstd_compress_stream) + ZSTD_CStreamOutSize());
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
bgl_zstd_compress_input_remaining(zstd_compress_streamp zstream) {
  obj_t res = BFALSE;

  if (zstream->input.pos < zstream->input.size) {
    printf("Joe D. -- input remaining\n");
    res = BTRUE;
  }
  
  return res;
}

BGL_RUNTIME_DEF obj_t
bgl_zstd_stream_compress(zstd_compress_streamp zstream, unsigned char* buffer, long length, obj_t finishp) {
  obj_t res = COMPRESS_CONTINUE;

  ZSTD_EndDirective const mode = finishp == BTRUE ? ZSTD_e_end : ZSTD_e_continue;

  //printf("Joe D. -- Buffer length: %d\n", length);
  
  if (length > 0 || finishp == BTRUE) {
    zstream->input.src = buffer;
    zstream->input.pos = 0;
    zstream->input.size = length;
  }

  ZSTD_outBuffer output = { zstream->buffer, zstream->length, 0 };

  size_t remaining = ZSTD_compressStream2(zstream->cctx, &output , &(zstream->input), mode);

  //printf("Joe D. -- remaining: %d\n", remaining);
  if (ZSTD_isError(remaining)) {
     C_SYSTEM_FAILURE(BGL_IO_READ_ERROR,
                     "bgl_zstd_stream_compress",
                     (char*)ZSTD_getErrorName(remaining),
                     ELONG_TO_BELONG(remaining));   
  }

  //printf("Joe D. -- output.pos: %d", output.pos);
  if (output.pos > 0) {
    //printf("Joe D. -- returning compressed data\n");
    res = string_to_bstring_len(zstream->buffer, output.pos);
  } else if (zstream->input.pos < zstream->input.size) {
    //printf("Joe D. -- compress continuing\n");
    res = COMPRESS_CONTINUE;
  } else if (remaining == 0 && finishp == BTRUE) {
    res = COMPRESS_DONE;
  }

  //printf("Joe D. -- returning data\n");
  return res;
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
bgl_zstd_close_input_stream(zstd_decompress_streamp zstream) {
      ZSTD_freeDCtx(zstream->dctx);
      return BUNSPEC;
}

BGL_RUNTIME_DEF obj_t
bgl_zstd_close_output_stream(zstd_compress_streamp zstream) {
      ZSTD_freeCCtx(zstream->cctx);
      return BUNSPEC;
}
