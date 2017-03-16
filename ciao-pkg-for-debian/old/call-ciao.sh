#!/bin/sh

# Call from main directory package.
export CIAOALIASPATH=`pwd`/ciao/library:`pwd`/ciao/lib 
export CIAOLIB=`pwd`/ciao/lib:`pwd`/ciao/library 

./ciao/bin/ciaoengine "$@" -C -b `pwd`/bin/ciaoc-1.13
