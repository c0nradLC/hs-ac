haskell_lib_name=libachook.so
cpp_lib_name=libac_hook.so

mkdir -p $PWD/out &&
cabal build --ghc-options="-shared -fPIC -dynamic -lHSrts-ghc9.2.8 -ldl -o $PWD/out/$haskell_lib_name -no-hs-main" &&
g++ $PWD/out/$haskell_lib_name -shared -fPIC $PWD/src/wrapper.cpp -o $PWD/out/$cpp_lib_name -ldl