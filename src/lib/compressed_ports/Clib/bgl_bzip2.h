#ifndef BGL_BZIP2_H
#define BGL_BZIP2_H

struct bzip2_compress_stream;
struct bzip2_decompress_stream;

typedef struct bzip2_compress_stream *bzip2_compress_streamp;
typedef struct bzip2_decompress_stream *bzip2_decompress_streamp;

BGL_RUNTIME_DECL void
bgl_bzip2_init();

BGL_RUNTIME_DECL bzip2_compress_streamp
bgl_bzip2_create_compress_stream(int block_size, obj_t output);

BGL_RUNTIME_DECL bzip2_decompress_streamp
bgl_bzip2_create_decompress_stream();

BGL_RUNTIME_DECL obj_t
bgl_bzip2_stream_compress(bzip2_compress_streamp stream,
                         unsigned char* buffer,
                         long length, obj_t finishp);

BGL_RUNTIME_DECL obj_t
bgl_bzip2_stream_decompress(bzip2_decompress_streamp stream, unsigned char *buffer,
                            long length);

BGL_RUNTIME_DECL obj_t
bgl_bzip2_close_decompress_stream(bzip2_decompress_streamp stream);

BGL_RUNTIME_DECL obj_t
bgl_bzip2_close_compress_stream(bzip2_compress_streamp stream);

#endif /* BGL_BZIP2_H */
