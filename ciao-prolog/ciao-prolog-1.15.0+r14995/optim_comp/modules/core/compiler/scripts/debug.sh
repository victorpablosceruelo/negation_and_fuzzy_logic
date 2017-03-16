#!/bin/sh
get_mydir() {
  old_dir=`pwd`
  cd `dirname $0`
  dir=`pwd`
  cd ${old_dir}
  echo ${dir}
}
self=`get_mydir`
## Body
if [ -x ${self}/arch ]; then
  true
else
  ${self}/compile_native --debuglevel=debug || exit -1
fi
echo "{Type 'run' to start the program}"
CIAOCCONFIG=${self}/configuration gdb --silent -d ${self}/c/engine --args ${self}/arch "$@" -C ${self}/noarch ${CIAORTOPTS}

