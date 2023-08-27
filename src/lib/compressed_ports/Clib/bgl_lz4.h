#ifndef BGL_LZ4_H
#define BGL_LZ4_H

#define LZ4_CHUNK_SIZE (16*1024)

struct lz4_compress_stream;
struct lz4_decompress_stream;

typedef struct lz4_compress_stream* lz4_compress_streamp;
typedef struct lz4_decompress_stream* lz4_decompress_streamp;

BGL_RUNTIME_DECL
void bgl_lz4_init();

BGL_RUNTIME_DECL lz4_compress_streamp
bgl_lz4_create_compress_stream(obj_t output);

BGL_RUNTIME_DECL lz4_decompress_streamp
bgl_lz4_create_decompress_stream();

BGL_RUNTIME_DECL obj_t
bgl_lz4_stream_compress(lz4_compress_streamp stream, unsigned char *buffer,
                        long length, obj_t finishp);
BGL_RUNTIME_DECL obj_t
bgl_lz4_stream_decompress(lz4_decompress_streamp stream, unsigned char *buffer,
                          long length);

BGL_RUNTIME_DECL obj_t
bgl_lz4_close_decompress_stream(lz4_decompress_streamp stream);

BGL_RUNTIME_DECL obj_t
bgl_lz4_close_compress_stream(lz4_compress_streamp stream);

#endif /* BGL_LZ4_H */
