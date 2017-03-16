#!/bin/sh

VERSIONS="0 1 2 3 4 5 6 7 8 9"

compile() {
    suf=$1
    for i in ${VERSIONS}; do
	echo "Compiling version $i (suf=${suf})"
	./do.sh ${i} compile${suf}
    done
}

checkmod() {
    suf=$1
    for i in ${VERSIONS}; do
	echo "Checking version $i (suf=${suf})"
	./do.sh ${i} checkmod
    done
}

savemod() {
    suf=$1
    for i in ${VERSIONS}; do
	echo "Saving version $i (suf=${suf})"
	./do.sh ${i} savemod
    done
}

sendgumstix() {
    suf=$1
    for i in ${VERSIONS}; do
	echo "Sending to gumstix $i (suf=${suf})"
	./do.sh ${i} sendgumstix
    done
}

bench() {
    suf=$1
    for i in ${VERSIONS}; do
	echo "Benchmark $i (suf=${suf})"
	./do.sh ${i} bench${suf}
    done
}

run() {
    suf=$1
    for i in ${VERSIONS}; do
	echo "Run $i (suf=${suf})"
	./do.sh ${i} run${suf}
    done
}

help() {
    cat <<EOF
Use options [compile|bench|run|compilegumstix|benchgumstix|sendgumstix|rungumstix]
EOF
}

case $1 in
    checkmod) checkmod ;;
    savemod) savemod ;;
    compile) compile ;;
    bench) bench ;;
    run) run ;;
    compilegumstix) compile gumstix ;;
    sendgumstix) sendgumstix ;;
    benchgumstix) bench gumstix ;;
    rungumstix) run gumstix ;;
    *) help ;;
esac

