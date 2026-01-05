/* wrapper.c */
#include <dlfcn.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

typedef void* SDL_Window;

/* Haskell runtime initialization */
extern "C" void hs_init(int *argc, char **argv[]);

/* Haskell replacement function */
extern "C" void sdlGLSwapWindowHook(SDL_Window *window);

/* vec def 
   had to make this wrapper use cpp instead of c in order for this vec struct to be understood by the game's IsVisible
*/
struct vec
{
    union
    {
        struct { float x, y, z; };
        float v[3];
        int i[3];
    };
};

/* Original function pointers */
static void (*real_SDL_GL_SwapWindow)(SDL_Window *window) = NULL;

extern "C" typedef uint8_t (*IsVisible_t)(vec from, vec to, void *tracer, bool skipTags);

/* Our replacements */
extern "C" void SDL_GL_SwapWindow(SDL_Window *window) {
    static bool hs_initialized = false;

    if (!hs_initialized) {
        hs_init(NULL, NULL);
        hs_initialized = true;
    }

    // Needs to be called before the actual SwapWindow otherwise ESP doesn't get drawn
    sdlGLSwapWindowHook(window);

    if (!real_SDL_GL_SwapWindow) {
        real_SDL_GL_SwapWindow = (void(*) (void**))dlsym(RTLD_NEXT, "SDL_GL_SwapWindow");
    }

    if (real_SDL_GL_SwapWindow) {
        real_SDL_GL_SwapWindow(window);
    }
}

extern "C" bool IsVisible(uintptr_t isVisibleFunctionAddr, float x1, float y1, float z1,
                        float x2, float y2, float z2)
{
    IsVisible_t real_IsVisible = (IsVisible_t)isVisibleFunctionAddr;

    vec v1 = {x1, y1, z1};
    vec v2 = {x2, y2, z2};

    return real_IsVisible(v1, v2, NULL, false);
}