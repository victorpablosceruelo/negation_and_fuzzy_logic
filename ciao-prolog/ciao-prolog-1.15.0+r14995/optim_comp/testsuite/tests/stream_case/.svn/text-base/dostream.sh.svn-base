#!/bin/sh
ciaotool try --bootstrap ptoc_stream ../stream.pl
ciaotool try --bootstrap ptoc_stream_2 ../stream_2.pl
ciaotool try --bootstrap ptoc_stream_3 ../stream_3.pl
gcc -O3 -o ptoc_stream_c ptoc_stream_c.c -lm
gcc -O3 -o ptoc_stream_c_array ptoc_stream_c_array.c -lm
