ESHOP_PATH="/home/wolfor/cl-restas-eshop/"
CONFIG_PATH=$ESHOP_PATH"config-320-release.eshop"
LIBS_PATH="/home/wolfor/cl-restas-eshop/libs/"

export CONFIG_PATH
export LIBS_PATH
export ESHOP_PATH

sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 2024 --load "$ESHOP_PATH"start-eshop.lisp
