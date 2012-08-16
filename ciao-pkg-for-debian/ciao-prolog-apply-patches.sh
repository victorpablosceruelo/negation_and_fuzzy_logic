#!/bin/bash

echo "running $0 $* ... "

if [ -z $1 ] || [ "$1" == "" ]; then
        echo "usage: $0 path_of_ciao_prolog_trunk"
        exit 0
fi


pushd ${1}
# Apply patches to the ciao distribution.
echo " -> Applying patches in debian/patches to ciao distribution ..."
for file in debian/patches/*
do
        if [ ! -d $file ] && [ ! "$file" == "." ] && [ ! "$file" == ".." ]; then
                echo "Found file patch in $file"
        #       patch -p0 --forward --verbose < $file
        fi
done
echo " "
popd

