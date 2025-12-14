## How to build and run
1. `cabal build --ghc-options="-shared -fPIC -dynamic -lHSrts-ghc9.2.8 -ldl -o ./out/libachook.so -no-hs-main -optl -W"`
2. `gcc -shared -fPIC src/wrapper.c -o ./out/libac_hook.so ./out/libachook.so -ldl`
3. `sudo cp ./out/libachook.so /lib/`     OR move it somewhere in LD_LIBRARY_PATH
4. run with LD_PRELOAD: `LD_PRELOAD=$PWD/out/libac_hook.so <path to AC binary>`
