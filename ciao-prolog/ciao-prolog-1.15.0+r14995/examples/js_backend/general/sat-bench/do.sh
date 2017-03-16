#!/bin/sh

#pushd sat-ciao > /dev/null
echo ">> SAT solver - Ciao version"
ciaoc sat_bench
echo "(executing...)"
./sat_bench

echo ">> SAT solver - Ciao [optim_comp] version"
ciaotool comp --dynexec sat_bench sat_bench
echo "(executing...)"
./sat_bench
#popd > /dev/null

#pushd sat-pl2js > /dev/null
echo ">> SAT solver - pl2js version"
ciaotool js-backend --target-platform nodejs check-exec sat_bench
echo "(executing...)"
ciaotool js-backend --target-platform nodejs run-exec sat_bench
#popd > /dev/null
