/* wrapper.c */
#include <dlfcn.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

typedef void* SDL_Window;

/* Haskell runtime initialization */
extern "C" void hs_init(int *argc, char **argv[]);

/* Haskell functions */
extern "C" void sdlGLSwapWindowHook(SDL_Window *window);
extern "C" void patchClient();

/* vec definition copied from AC's geom.h, needs to be the same vec struct in order for IsVisible to calculate things properly */
struct vec
{
    union
    {
        struct { float x, y, z; };
        float v[3];
        int i[3];
    };
};

class playerent
{
    public:
    char teamPadding[0x320]; // Padding for offset alignment when "converting" AC's playerent to our playerent. Needed for playerincrosshair.
    int team;
};

static bool hs_initialized = false;

/* Original function pointers */
static void (*real_SDL_GL_SwapWindow)(SDL_Window *window) = NULL;

extern "C" typedef uint8_t (*IsVisible_t)(vec from, vec to, void *tracer, bool skipTags);
extern "C" typedef void (*Attack_t)(bool attack);
extern "C" typedef playerent* (*PlayerInCrosshair_t)();

void __attribute__ ((constructor)) setup() {
    if (!hs_initialized) {
        hs_init(NULL, NULL);
        hs_initialized = true;
    }
    
    patchClient();
}

/* Our SDL_GL_SwapWindow hook/replacement */
extern "C" void SDL_GL_SwapWindow(SDL_Window *window) {
    if (!hs_initialized) {
        hs_init(NULL, NULL);
        hs_initialized = true;
    }

    // Needs to be called before the actual SwapWindow otherwise ESP boxes dont get drawn
    sdlGLSwapWindowHook(window);

    if (!real_SDL_GL_SwapWindow) {
        real_SDL_GL_SwapWindow = (void(*) (void**))dlsym(RTLD_NEXT, "SDL_GL_SwapWindow");
    }

    if (real_SDL_GL_SwapWindow) {
        real_SDL_GL_SwapWindow(window);
    }
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

extern "C" void attack(uintptr_t attackFunctionAddr, bool attack)
{
    Attack_t real_Attack = (Attack_t)attackFunctionAddr;

    real_Attack(attack);
}

extern "C" int playerincrosshair(uintptr_t playerInCrosshairFunctionAddr)
{
    PlayerInCrosshair_t real_PlayerInCrosshair = (PlayerInCrosshair_t)playerInCrosshairFunctionAddr;

    playerent* playerAimedAt = real_PlayerInCrosshair();

    if (playerAimedAt)
    {
        return playerAimedAt->team;
    }
    else
    {
        return -1;
    }
}