#!/bin/bash

echo "running $0 $* ... "

if [ -z $1 ] || [ "$1" == "" ] || [ -z $2 ] || [ "$2" == "" ]; then
        echo "usage: $0 path_of_ciao_prolog_trunk (apply|do_not_apply)"
        exit 0
fi


pushd ${1}
# Apply patches to the ciao distribution.
echo " -> Applying patches in debian/patches to ciao distribution ..."
for file in debian/patches/*
do
        if [ ! -d $file ] && [ ! "$file" == "." ] && [ ! "$file" == ".." ]; then
                echo "Found file patch in file $file"
		if [ "$2" == "apply" ]; then
		    echo "Applying patch in file $file"
		    patch -p0 --forward --verbose < $file
		else
		    echo "Not applied patch in file $file. Disabled functionality."
		fi
        fi
done
echo " "
popd

