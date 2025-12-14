/* wrapper.c */
#include <dlfcn.h>
#include <stdio.h>

typedef void* SDL_Window;

/* Haskell runtime initialization */
void hs_init(int *argc, char **argv[]);

/* Haskell functions */
void sdlGLSwapWindowHook(SDL_Window *window);

/* Original function pointers */
static void (*real_SDL_GL_SwapWindow)(SDL_Window *window) = NULL;

/* Our replacement */
void SDL_GL_SwapWindow(SDL_Window *window) {
    hs_init(NULL, NULL);

    if (!real_SDL_GL_SwapWindow) {
        real_SDL_GL_SwapWindow = dlsym(RTLD_NEXT, "SDL_GL_SwapWindow");
    }

    if (real_SDL_GL_SwapWindow) {
        real_SDL_GL_SwapWindow(window);
    }

    sdlGLSwapWindowHook(window);
}