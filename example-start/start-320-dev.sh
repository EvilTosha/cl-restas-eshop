ESHOP_PATH="/home/webadmin/eshop-dev/"
CONFIG_PATH=$ESHOP_PATH"config-320-dev.eshop"
LIBS_PATH="/home/webadmin/eshop/libs/"

export CONFIG_PATH
export LIBS_PATH
export ESHOP_PATH

screen -dmS wolfor-sbcl sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 1024 --load "$ESHOP_PATH"/start-eshop.lisp
