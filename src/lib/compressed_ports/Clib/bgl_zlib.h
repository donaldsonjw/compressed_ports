#ifndef BGL_ZLIB_H
#define BGL_ZLIB_H


struct zlib_stream; 

typedef struct zlib_stream* zlib_streamp;


BGL_RUNTIME_DECL void
bgl_zlib_init();

BGL_RUNTIME_DECL zlib_streamp
bgl_zlib_create_inflate_stream(obj_t gzip_wrappedp);


BGL_RUNTIME_DECL zlib_streamp
bgl_zlib_create_deflate_stream(int level, obj_t gzip_wrappedp);

BGL_RUNTIME_DECL obj_t
bgl_zlib_stream_inflate(zlib_streamp zstream,
                        unsigned char *buffer,
                        long length);

BGL_RUNTIME_DECL obj_t
bgl_zlib_stream_deflate(zlib_streamp zstream, unsigned char* buffer, long length,
                        obj_t finishp);

BGL_RUNTIME_DECL obj_t
bgl_zlib_close_input_stream(zlib_streamp zstream);

BGL_RUNTIME_DECL obj_t
bgl_zlib_close_output_stream(zlib_streamp zstream);


#endif /* BGL_GZIP_H */ 
