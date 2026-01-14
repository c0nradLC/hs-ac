/* wrapper.c */
#include <dlfcn.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

/* Haskell runtime initialization */
extern "C" void hs_init(int *argc, char **argv[]);

struct vec
{
    union
    {
        struct { float x, y, z; };
        float v[3];
        int i[3];
    };
};

/* Haskell functions */
extern "C" void sdlGLSwapWindowHook(void* window);
extern "C" void patchClient();

static bool hs_initialized = false;

/* Original function pointers */
static void (*real_SDL_GL_SwapWindow)(void *window) = NULL;

extern "C" typedef uint8_t (*IsVisible_t)(vec from, vec to, void *tracer, bool skipTags);

void __attribute__ ((constructor)) setup() {
    hs_init(NULL, NULL);
    hs_initialized = true;

    patchClient();
}

/* Our SDL_GL_SwapWindow hook/replacement */
extern "C" void SDL_GL_SwapWindow(void *window) {
    if (!hs_initialized) {
        hs_init(NULL, NULL);
        hs_initialized = true;
    }

    sdlGLSwapWindowHook(window);
}

/* Our direct calls to AC's functions */
extern "C" bool isvisible(uintptr_t isVisibleFunctionAddr, float x1, float y1, float z1,
                        float x2, float y2, float z2)
{
    IsVisible_t real_IsVisible = (IsVisible_t)isVisibleFunctionAddr;

    vec v1 = {x1, y1, z1};
    vec v2 = {x2, y2, z2};

    return real_IsVisible(v1, v2, NULL, false);
}