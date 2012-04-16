HOME_PATH="/home/webadmin/"
ESHOP_PATH=$HOME_PATH"cl-restas-eshop/"
CONFIG_PATH=$ESHOP_PATH"release-config.eshop"
LIBS_PATH=$ESHOP_PATH"libs/"
#TEMPLATES_PATH=$HOME_PATH"Dropbox/httpls/release/"

export CONFIG_PATH
export LIBS_PATH
export ESHOP_PATH

sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 1024 --load "$ESHOP_PATH"start-eshop.lisp
