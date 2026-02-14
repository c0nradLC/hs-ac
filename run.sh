export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD &&
GIMME=${2:-'all'} LD_PRELOAD=$PWD/libachook.so $1