#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

/* vec struct model necessary for all the calcs it does */
struct vec
{
    union
    {
        struct { float x, y, z; };
        float v[3];
        int i[3];
    };
};

/* Haskell RTS initialization */
void hs_init(int *argc, char **argv[]);
/* Haskell SDL_GL_SwapWindow hook */
void sdlGLSwapWindowHook(void* window);
/* Haskell patchClient, rnu only once by constructor */
void patchClient();

/* Function pointer definition for IsVisible */
typedef bool (*IsVisibleFunc)(struct vec, struct vec, void *, bool); 

void __attribute__ ((constructor)) setup() {
    /* Initialize Haskell's RTS */
    hs_init(NULL, NULL);
    /* Patch client functions and store static pointers and addresses in Haskell's IORefs inside the Global module */
    patchClient();
}

/*  Our SDL_GL_SwapWindow hook/replacement

    Had to put the hook definition in the C wrapper because Haskell RTS needs to be initialized
    before any functions get called, and we can't declare a C constructor as
    'foreign export ccall "__attribute__ ((constructor)) setup()"' in the Haskell lib
    even if we declare a 'foreign export ccall "SDL_GL_SwapWindow" func :: Ptr() -> IO(),
    it would hook/replace the original SDL_GL_SwapWindow but it would error out due to Haskell's RTS not being initialized.

    This wrapper really is just a wrapper.

    Btw we hook SDL_GL_SwapWindow because the current AC version (1.3.0.2) uses libSDL2-2.0, which is
    an SDL version that already replaced SDL_GL_SwapBuffers by SDL_GL_SwapWindow */
void SDL_GL_SwapWindow(void *window)
{
    sdlGLSwapWindowHook(window);
}

/*  Needs to be called from C because the function expects vec params
    as value instead of pointers and Haskell's FFI can't pass non-primitive
    value params directly to function pointers due to how it handles data marshalling.
    In the end it might even be possible somehow, but I didn't investigate it to that extent,
    it's good enough this way imho. */
bool isvisible(IsVisibleFunc isVisibleFunctionAddr, struct vec *vf, struct vec *vt)
{
    return isVisibleFunctionAddr(*vf, *vt, NULL, false);
}