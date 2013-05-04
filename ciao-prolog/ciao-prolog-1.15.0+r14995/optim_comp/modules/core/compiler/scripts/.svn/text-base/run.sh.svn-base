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
  ${self}/compile_native || exit 1
fi
CIAOCCONFIG=${self}/configuration ${self}/arch "$@" -C ${self}/noarch ${CIAORTOPTS}
