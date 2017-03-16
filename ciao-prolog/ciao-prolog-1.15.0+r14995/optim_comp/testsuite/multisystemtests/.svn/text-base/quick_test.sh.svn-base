#!/bin/bash

# Obtain the directory where this script is located
old_dir=`pwd`; cd `dirname $0`; self=`pwd`; cd ${old_dir}; old_dir=

# ---------------------------------------------------------------------------
# A simple multisystem performance benchmark
# (jfmc)

ocdir=${self}/../..

TESTS="\
    mtsys_boyer\
    mtsys_crypt\
    mtsys_deriv\
    mtsys_exp\
    mtsys_factorial\
    mtsys_fft\
    mtsys_fib\
    mtsys_knights\
    mtsys_nreverse\
    mtsys_poly\
    mtsys_primes\
    mtsys_qsort\
    mtsys_queens11\
    mtsys_query\
    mtsys_tak"

function do_tests { # arguments: system
    local sys
    sys=$1
    for i in $TESTS; do
        pushd ${ocdir}/testsuite/tests/$i > /dev/null
        ${ocdir}/ciaotool bench mtsys-evalmod-${sys} $i
        popd > /dev/null
    done
}

#for sys in swiprolog ciao; do
#for sys in swiprolog ciao ciao_1_6; do
#for sys in ciao2; do
for sys in ciao; do
    do_tests ${sys}
done
