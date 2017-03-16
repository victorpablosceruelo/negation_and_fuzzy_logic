#!/bin/sh

for i in c_1 c_2 c_3 c_4 c_5 c_6; do
    echo "(Compiling $i)"
    ciaotool comp --dynexec ${i} ${i}
    echo "(Executing $i)"
    ./${i}
done
