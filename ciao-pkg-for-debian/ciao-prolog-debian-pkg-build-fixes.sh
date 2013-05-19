#!/bin/bash

echo "running $0 $* ... "

if [ -z $1 ] || [ "$1" == "" ]; then
	echo "usage: $0 path_ciao_prolog_de "
	exit 0
fi

pushd ${1}

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

echo "FIXES:"
# nocompile_nor_distribute ciao/contrib/clpfd
# nocompile_nor_distribute ciao/contrib/difference_constraints
# nocompile_nor_distribute ciao/contrib/ppl/0_10
# nocompile_nor_distribute ciao/contrib/ppl/0_9
# rm -fv ciao/contrib/cneg/NOCOMPILE ciao/contrib/cneg/NODISTRIBUTE

# Do not include my packages in Ciao Prolog. I'll do it by hand.
rm -frv ciao/contrib/cneg
rm -frv ciao/contrib/rfuzzy
rm -frv ciao/contrib/pkgs_output_debug

popd
# EOF

