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
rm -f ${self}/arch ${self}/o/*.o ${self}/compile_native_aux.c ${self}/compile_native_aux
