HOME_PATH="/home/eviltosha/"
ESHOP_PATH=$HOME_PATH"cl-restas-eshop/"
CONFIG_PATH=$ESHOP_PATH"example-start/tosha-config.eshop"
LIBS_PATH=$ESHOP_PATH"libs/"
SWANK_PORT=4005

export CONFIG_PATH
export LIBS_PATH
export ESHOP_PATH
export SWANK_PORT

sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 1024 --load "$ESHOP_PATH"start-eshop.lisp
