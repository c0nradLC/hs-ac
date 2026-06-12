# hs-ac
#### Internal game hack for Assault Cube v1.3.0.2 on linux.

<p align="center">
  <img alt="screenshot of the game with the cheat running" src="./media/image.png">
</p>

This project is nothing but an experiment on learning the basics of reverse engineering and game hacking on linux, it is not intended to be commercialized in any way or used in the game's multiplayer matches.

## Cheats
- Hit-kill
- Invincibility
- Magnet
- Aimbot
- Triggerbot
- Sight-kill
- No Recoil
- No Spread
- No knockback
- Infinite ammo

## Setup
### Building
Run `./build.sh`, this should produce two shared libraries: `libachook.so` and `libhs-ac.so`.

`libhs-ac.so` is the shared lib produced by Haskell, while `libachook.so` is produced by the C wrapper, which uses `libhs-ac.so` as an input.

### Running
You should be at the root of this repo and call `./run.sh  <path-to-assaultcube.sh> <cheats-you-want>`.

The cheat uses the `LD_PRELOAD` technique to hook the `SDL_GL_SwapWindow` function from `libSDL`, both `libhs-ac.so` and `libachook.so` libs must be "findable" by your system, `run.sh` sets this up by appending `$PWD` to `LD_LIBRARY_PATH`. The script should work anywhere given the shared libs are in the same path as `run.sh`.

## Thanks
This project was heavily inspired by the [`Guided Hacking`](https://www.youtube.com/@GuidedHacking) yt channel, ever since I was a kid I wanted to create my own cheats thanks to this guy's videos, huge thanks to this legend and all the game hacking and reverse engineering community.

The inspiration for writing this cheat in Haskell and for the linux version came from [`scannells - ac_rhack`](https://github.com/scannells/ac_rhack) project, huge thanks to this guy for unknowingly planting this idea in my head.

Thank **you** for accessing this repo, I hope this inspires you to also **learn** the basics of reverse engineering and game hacking, it's actually way funnier and rewarding than you might expect.

# **This software contains no Assault Cube intellectual property and is not affiliated in any way.**