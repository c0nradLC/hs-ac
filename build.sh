cabal build --enable-shared &&
gcc hs-ac.so -shared -fPIC ./src/wrapper.c -o libachook.so -ldl
