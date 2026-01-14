export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD/out &&
LD_PRELOAD=$PWD/out/libac_hook.so $1
