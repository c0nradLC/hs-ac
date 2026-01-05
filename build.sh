cabal build --ghc-options="-shared -fPIC -dynamic -lHSrts-ghc9.2.8 -ldl -o ./out/libachook.so -no-hs-main -optl -W" &&
g++ -shared -fPIC src/wrapper.cpp -o ./out/libac_hook.so ./out/libachook.so -ldl &&
sudo cp $PWD/out/libachook.so /lib/