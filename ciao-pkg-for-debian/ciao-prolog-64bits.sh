#!/bin/bash

echo " "
echo "(estos comandos ponen variables de entorno que deben"
echo " estar siempre activas cuando llames a 'ciaosh')"
echo " "
echo "eval \`./ciaotool bash-env\`"
echo " "
pushd ~/secured/tests/CiaoDE_trunk/optim_comp 2>&1 >/dev/null
echo "`./ciaotool bash-env`"
popd 2>&1 >/dev/null
echo "export ABSMACH_OPTGRP=tags"
echo "export ABSMACH_OPTS=tagscheme28"
echo " "
echo "(estos comandos generan el ciao de 64 bits) "
echo " "
echo "ciaotool update-comp"
echo "ciaotool update-all"
echo " "

echo "TEST: set | grep \"\(ABS\|CIAO\)\""
echo " "
set | grep "\(ABS\|CIAO\)"
echo " "

if [ -z "$1" ] || [ "$1" == "" ]; then 
	exit 0
fi

TEST=`set | grep "\(ABS\|CIAO\)"`
if [ -z "$TEST" ] || [ "$TEST" == "" ]; then
	echo "Environment variables not set-up."
	exit 0
fi

pushd ~/secured/tests/CiaoDE_trunk
pushd optim_comp

eval `./ciaotool bash-env`
export ABSMACH_OPTGRP=tags
export ABSMACH_OPTS=tagscheme28

ciaotool update-comp
ciaotool update-all

popd
popd
echo "El toplevel se arranca con 'ciaosh'."
