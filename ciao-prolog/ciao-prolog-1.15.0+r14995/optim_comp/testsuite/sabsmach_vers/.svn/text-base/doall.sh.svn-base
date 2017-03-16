#!/bin/bash

tempinput="/tmp/rep.txt"
tempinput2="/tmp/rep2.txt"

# ---------------------------------------------------------------------------
what=tagschemes
echo "Generating view and summary for ${what}"

machines="ciempies x86_64 clip huygens"
echo > ${tempinput}

mkdir -p out_view/${what}
for machine in ${machines}; do
    # TODO: merge summary and view
    echo "machine(${machine})." >> ${tempinput}
    gunzip -c ${what}_results/${machine}.txt.gz > ${tempinput2}
    ciaotool tests sabsmach-vers summary preprocess ${tempinput2} >> ${tempinput}
    ciaotool tests sabsmach-vers summary compact ${tempinput2} > out_view/${what}/summary_${machine}_compact.txt
    ciaotool tests sabsmach-vers summary full ${tempinput2} > out_view/${what}/summary_${machine}_full.txt
done
./view.sh ${what} ${tempinput} out_view/${what} plots
./view.sh ${what} ${tempinput} out_view/${what} html
rm ${tempinput}
rm ${tempinput2}

# ---------------------------------------------------------------------------
what=generalold
echo "Generating view for ${what}"

machines="ppc clop"
echo > ${tempinput}

mkdir -p out_view/${what}
for machine in ${machines}; do
    # TODO: convert old data
    echo "machine(${machine})." >> ${tempinput}
    cat ${what}_results/${machine}_rep.txt >> ${tempinput}
done
./view.sh ${what} ${tempinput} out_view/${what} plots
./view.sh ${what} ${tempinput} out_view/${what} html
rm ${tempinput}

