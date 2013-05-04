#!/bin/sh

compile_prolog() {
    ciaotool try --bootstrap ptoc_stream_3 ../stream_3.pl
}

compile_c_local() {
    ./ptoc_stream_3.car/clean
    ./ptoc_stream_3.car/compile_native
    gcc -O3 -o ptoc_stream_c ptoc_stream_c.c -lm
}

compile_c_gumstix() {
    ./ptoc_stream_3.car/clean
    ./ptoc_stream_3.car/compile_native --remote=root@gumstix
    arm-linux-gcc -O3 -o ptoc_stream_c ptoc_stream_c.c -lm
}

run_local() {
    time ./ptoc_stream_c
    export LOCALSTKSIZE=4000000
    export CHOICESTKSIZE=4000000
    export GLOBALSTKSIZE=22000000
    time ./ptoc_stream_3.car/run
    time ./ptoc_stream_3.car/run
}

run_gumstix() {
    echo "Copying to gumstix..."
    scp ptoc_stream_3.car/arch ptoc_stream_3.car/noarch root@gumstix:/tmp
    echo "Executing"
    ssh root@gumstix /tmp/arch -C -b /tmp/noarch
}

help() {
    cat <<EOF
Use 'local' to compile and run the test locally.
Use 'gumstix' to compile and run the test in the gumstix.
Use 'hack' to compile C code and run the test (useful when changing the generated C code by hand).
EOF
}

case $1 in
    local) compile_prolog && compile_c_local && run_local ;;
    gumstix) compile_prolog && compile_c_gumstix && run_gumstix ;;
    run_local) run_local ;;
    run_gumstix) run_gumstix ;;
    hack_local) compile_c_local && run_local ;;
    *) help ;;
esac