#!/bin/sh

case $1 in
    compile)
      gcc -Wall -O2 -fomit-frame-pointer compass_simul_handwritten.c -o out/handmade -lm
      ;;
    run)
      out/handmade ~/svn/asap_stream_interpreter/Compass/Binaries/dies_irae_44100_16s.sw | out/to_dsp -b 9 -r 44100
      ;;
    bench)
      time out/handmade ~/svn/asap_stream_interpreter/Compass/Binaries/dies_irae_44100_16s.sw > /dev/null
      ;;
    *)
      cat <<EOF
Usage: $(basename $0) [compile|run|bench]

This benchmark tests a hand-made version of the compass operationally
equivalent to compass_simul. It uses dynamic memory and an ad-hoc
algorithm to recover memory.

EOF
    ;;
esac