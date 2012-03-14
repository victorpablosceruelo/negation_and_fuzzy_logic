#!/bin/bash

pushd ~/secured/tests/CiaoDE_trunk
pushd optim_comp

#(estos comandos ponen variables de entorno que deben
#estar siempre activas cuando llames a 'ciaosh')
eval `./ciaotool bash-env`
export ABSMACH_OPTGRP=tags
export ABSMACH_OPTS=tagscheme28

ciaotool update-comp
ciaotool update-all

echo "El toplevel se arranca con 'ciaosh'."
