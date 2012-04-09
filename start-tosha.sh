CONFIG_PATH="/home/eviltosha/cl-restas-eshop/config.eshop"
LIBS_PATH="/home/eviltosha/cl-restas-eshop/libs/"
ESHOP_PATH="/home/eviltosha/cl-restas-eshop/"
export CONFIG_PATH
export LIBS_PATH
export ESHOP_PATH

sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 1024 --load ~/cl-restas-eshop/example-start/start-eshop.lisp
