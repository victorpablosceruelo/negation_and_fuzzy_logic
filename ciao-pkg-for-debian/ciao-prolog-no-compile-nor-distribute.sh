#!/bin/bash

if [ -z $1 ] || [ "$1" == "" ]; then
	echo "usage: $0 path_of_folder_to_nocompile_and_nodistribute"
	exit 0
fi


function nocompile_nor_distribute () {
    if [ ! -z "$1" ] && [ ! "$1" == "" ] && [ -d "$1" ]; then
        touch $1/NOCOMPILE
        pushd $1
        ls -1 *.pl > NOCOMPILE 2>&1
        popd
        touch $1/NODISTRIBUTE
    else
        echo "Erroneous folder: $1"
    fi
}

nocompile_nor_distribute $1
