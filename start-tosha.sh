HOME_PATH="/home/eviltosha/"
ESHOP_PATH=$HOME_PATH"cl-restas-eshop/"
CONFIG_PATH=$ESHOP_PATH"config.eshop"
LIBS_PATH=$ESHOP_PATH"libs/"
#TEMPLATES_PATH=$HOME_PATH"Dropbox/httpls/release/"

export CONFIG_PATH
export LIBS_PATH
export ESHOP_PATH

sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 2024 --load "$ESHOP_PATH"example-start/start-eshop.lisp
