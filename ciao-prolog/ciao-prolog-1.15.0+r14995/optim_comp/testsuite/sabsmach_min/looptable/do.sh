#!/bin/sh
get_mydir() {
  old_dir=$(pwd)
  cd $(dirname $0)
  dir=$(pwd)
  cd ${old_dir}
  echo ${dir}
}
self=$(get_mydir)
## Body
gcc -Wall -o ${self}/looptable ${self}/looptable.c
${self}/looptable > tables.tex

