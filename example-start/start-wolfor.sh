ESHOP_PATH="/home/wolfor/cl-restas-eshop/"
CONFIG_PATH=$ESHOP_PATH"config.eshop"
LIBS_PATH=$ESHOP_PATH"libs/"

export CONFIG_PATH
export LIBS_PATH
export ESHOP_PATH

sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 1024 --load "$ESHOP_PATH"start-eshop.lisp
