#ifndef BGL_ZSTD_H
#define BGL_ZSTD_H

struct zstd_compress_stream;
struct zstd_decompress_stream;

typedef struct zstd_compress_stream* zstd_compress_streamp;
typedef struct zstd_decompress_stream* zstd_decompress_streamp;

BGL_RUNTIME_DECL zstd_compress_streamp
bgl_zstd_create_compress_stream(int level);
  
BGL_RUNTIME_DECL zstd_decompress_streamp
  bgl_zstd_create_decompress_stream();

BGL_RUNTIME_DECL obj_t
bgl_zstd_compress_input_remaining(zstd_compress_streamp zstream);

BGL_RUNTIME_DECL obj_t
bgl_zstd_stream_compress(zstd_compress_streamp zstream,
                         unsigned char* buffer, long length, obj_t finishp);

BGL_RUNTIME_DECL obj_t
bgl_zstd_stream_decompress(zstd_decompress_streamp zstream,
                           unsigned char *buffer, long length);

BGL_RUNTIME_DECL obj_t
bgl_zstd_close_input_stream(zstd_decompress_streamp zstream);

BGL_RUNTIME_DECL obj_t
bgl_zstd_close_output_stream(zstd_compress_streamp zstream);

BGL_RUNTIME_DECL long
bgl_zstd_input_buffer_size();

BGL_RUNTIME_DECL long
bgl_zstd_output_buffer_size();

BGL_RUNTIME_DECL void
bgl_zstd_init();

#endif /* BGL_ZSTD_H*/
