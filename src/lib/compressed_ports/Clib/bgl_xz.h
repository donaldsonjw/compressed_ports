#ifndef BGL_XZ_H
#define BGL_XZ_H

struct xz_compress_stream;
struct xz_decompress_stream;

typedef struct xz_compress_stream* xz_compress_streamp;
typedef struct xz_decompress_stream* xz_decompress_streamp;

BGL_RUNTIME_DECL void
bgl_xz_init();

BGL_RUNTIME_DECL xz_compress_streamp
bgl_xz_create_compress_stream(int preset, obj_t output);

BGL_RUNTIME_DECL xz_decompress_streamp bgl_xz_create_decompress_stream();

BGL_RUNTIME_DECL obj_t bgl_xz_stream_compress(xz_compress_streamp stream,
                                              unsigned char *buffer,
                                              long length, obj_t finishp);

BGL_RUNTIME_DECL obj_t bgl_xz_stream_decompress(xz_decompress_streamp stream,
                                                unsigned char *buffer,
                                                long length);

BGL_RUNTIME_DECL obj_t bgl_xz_close_output_stream(xz_compress_streamp stream);

BGL_RUNTIME_DECL obj_t
bgl_xz_close_input_stream(xz_decompress_streamp stream);

#endif /* BGL_XZ_H */ 
