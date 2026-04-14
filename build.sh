cabal build --enable-shared &&
cp ./dist-newstyle/build/x86_64-linux/ghc-9.8.4/hs-ac-0.1.0.0/f/hs-ac/build/hs-ac/libhs-ac.so.0.0.0 ./libhs-ac.so.0 &&
gcc ./libhs-ac.so.0 -shared -fPIC ./src/wrapper.c -o libachook.so -ldl
