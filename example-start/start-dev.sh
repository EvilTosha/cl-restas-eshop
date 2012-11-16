ESHOP_PATH="$HOME/eshop-dev/"
CONFIG_PATH=$ESHOP_PATH"example-start/config-320-dev.eshop"
LIBS_PATH=$HOME"eshop/libs/"
SWANK_PORT=7777

export ESHOP_PATH
export CONFIG_PATH
export LIBS_PATH
export SWANK_PORT

screen -dmS dev-eshop sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 1024 --load $ESHOP_PATH"start-eshop.lisp"
