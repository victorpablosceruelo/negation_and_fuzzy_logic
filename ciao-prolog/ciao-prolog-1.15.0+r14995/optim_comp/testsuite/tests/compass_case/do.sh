#!/bin/sh
get_mydir() {
  old_dir=$(pwd)
  cd $(dirname $0)
  dir=$(pwd)
  cd ${old_dir}
  echo ${dir}
}
self=$(get_mydir)
## Body

cpreprocess() {
#    export CIAOOPTS="--profile=yes $CIAOOPTS"
    echo "C-Preprocessing prg${VERSION}"
    echo "  optimize=${OPTIMIZE}, unfold=${UNFOLD}, realcomp=${REALCOMP}"
    # Preprocess compass_simul source using version options
    CPP_OPTIONS="-DPARAM_FREQUENCY=${FREQ} -DPARAM_COMPASS=${COMPASS} -DPARAM_AUDIO_PER_COMPASS=${AUDIO_PER_COMPASS} -DPARAM_OPTIMIZE=${OPTIMIZE} -DPARAM_UNFOLD=${UNFOLD} -DPARAM_REALCOMP=${REALCOMP}"
    cpp ${CPP_OPTIONS} -C -P < ${self}/compass_simul.pl > ${PROGRAM}.pl
    cp ${self}/io_access.pl ${OUTDIR}/io_access.pl
    if [ x"${REALCOMP}" == x1 ]; then
	# use real compass
	echo "  using real compass"
	echo "#define USE_REAL_COMPASS 1" > ${OUTDIR}/io_access_c.c
	cat ${self}/read_compass.c >> ${OUTDIR}/io_access_c.c
	cat ${self}/io_access_orig.c >> ${OUTDIR}/io_access_c.c
    else
	if [ x"${COMPASS}" == x10 ]; then
	    # use fast angle increments
	    echo "  using emulated compass with fast angle increments"
	    echo "#define USE_FAST_ANGLE 1" > ${OUTDIR}/io_access_c.c
	else
	    # use slow angle increments
	    echo "  using emulated compass with slow angle increments"
	    echo "" > ${OUTDIR}/io_access_c.c
	fi
	cat ${self}/io_access_orig.c >> ${OUTDIR}/io_access_c.c
    fi
}

checkmod() {
    # Do a fast module compilation and check it
    pushd ${OUTDIR} > /dev/null
    ciaotool bench checkmod "prg${VERSION}"
    popd > /dev/null
}

compilecommon() {
    NATIVE_OPTIONS="$*"
    # (this step should not be necessary, but it is better to do it...)
    checkmod
    # Compile program
    ciaotool try --bootstrap ${PROGRAM} ${PROGRAM} && \
    ${PROGRAM}.car/clean && \
    ${PROGRAM}.car/compile_native ${NATIVE_OPTIONS}
    echo "${SOUNDPATH}/${SOUNDFILE}" > ${PROGRAM}.input
    echo "-f 2 -s 14 -r ${FREQ}" > ${PROGRAM}.dspopts
}

compile() {
    # Compile program
    compilecommon
}

compilegumstix() {
    # Compile program
    compilecommon --remote=root@gumstix
}

savemod() {
    pushd ${OUTDIR} > /dev/null
    ciaotool bench savemod "prg${VERSION}"
    popd > /dev/null
}

run() {
    # Compile dsp program
    gcc -O2 ${self}/to_dsp.c -o ${OUTDIR}/to_dsp
    # 
    ${PROGRAM}.car/run $(cat ${PROGRAM}.input) &
    ${OUTDIR}/to_dsp $(cat ${PROGRAM}.dspopts) -h 127.0.0.1
}

bench() {
    # Compile dsp program with the BENCHMARK option (no sound is emitted)
    gcc -DBENCHMARK -O2 ${self}/to_dsp.c -o ${OUTDIR}/to_dsp
    # 
    time ${PROGRAM}.car/run $(cat ${PROGRAM}.input) &
    ${OUTDIR}/to_dsp $(cat ${PROGRAM}.dspopts) -h 127.0.0.1
}

sendgumstix() {
    scp ${PROGRAM}.car/arch root@gumstix:/tmp/arch${VERSION}
    scp ${PROGRAM}.car/noarch root@gumstix:/tmp/noarch${VERSION}
}

benchgumstix() {
    echo "This test is disabled"
    # time ssh root@gumstix "/root/time /tmp/arch${VERSION} /tmp/${SOUNDFILE} -C -b /tmp/noarch${VERSION} > /dev/null"
}

sndserver() {
    # Compile dsp program
    gcc -O2 ${self}/to_dsp.c -o ${OUTDIR}/to_dsp
    # Wait for incoming connections and send sound to_dsp
    # nc -v -l -p 3003 192.168.109.199 | ${OUTDIR}/to_dsp $(cat ${PROGRAM}.dspopts);
}

# rungumstix() {
#     # Run (the sndserver must be enabled)
#     time ssh root@gumstix "/root/time /tmp/arch${VERSION} /tmp/dies_irae_${FREQ}_16s.sw -C -b /tmp/noarch${VERSION} | nc 192.168.109.1 3003"
# }

# rungumstix() {
#     # Run (the sndserver must be enabled)
#     time ssh root@gumstix "/root/time /tmp/arch${VERSION} /tmp/bbc.sw -C -b /tmp/noarch${VERSION} | nc 192.168.109.1 3003"
# }

rungumstix() {
    # Run; this lanuches the sound client
    ssh -f root@gumstix "/tmp/arch${VERSION} /tmp/${SOUNDFILE} -C -b /tmp/noarch${VERSION}"
    ${OUTDIR}/to_dsp $(cat ${PROGRAM}.dspopts) -h 192.168.109.199
}


copysample() {
    scp "${SOUNDPATH}/${SOUNDFILE}" root@gumstix:/tmp
}

help() {
    cat <<EOF
Usage: $(basename $0) VERSION [realcomp] ACTION

This program compiles and tests the 'compass' benchmark. Actions can be:

  checkmod             compile and check module (ciaotool bench)
  savemod              save the compilation output (ciaotool bench)

  compile              compile the version
  run                  run the version
  bench                do a benchmark for the version
  save                 save the output of the benchmark
  check                check that the output is correct

Use the 'realcomp' option if you want to use a real compass instead
of a simulated one (only works in the Gumstix).

Or gumstix specific options:

  compilegumstix       compile for the Gumstix
  sendgumstix          send binaries to the Gumstix
  sndserver            compiles the sound server for the Gumstix
  rungumstix           run in the Gumstix
  benchgumstix         benchmark in the Gumstix
  copysample           send sound sample to the Gumstix
EOF
}

# Set options depending on the version
setopts() {
    case ${VERSION} in
	0) OPTIMIZE=0; UNFOLD=0 ;;
	1) OPTIMIZE=1; UNFOLD=0 ;;
	2) OPTIMIZE=2; UNFOLD=0 ;;
	3) OPTIMIZE=3; UNFOLD=0 ;;
	4) OPTIMIZE=4; UNFOLD=0 ;;
	5) OPTIMIZE=0; UNFOLD=1 ;;
	6) OPTIMIZE=1; UNFOLD=1 ;;
	7) OPTIMIZE=2; UNFOLD=1 ;;
	8) OPTIMIZE=3; UNFOLD=1 ;;
	9) OPTIMIZE=4; UNFOLD=1 ;;
    esac
    PROGRAM="${OUTDIR}/prg${VERSION}"
}

# Get version parameter
VERSION="${1}"
shift
# Get realcomp parameter
if [ x"${1}" == x"realcomp" ]; then
    REALCOMP=1
    shift
else
    REALCOMP=0
fi
# Initialize other parameters
SOUNDPATH=~/svn/asap_stream_interpreter/Compass/Binaries
SOUNDFILE=bbc.sw
FREQ=44100
COMPASS=10
#COMPASS=44100
AUDIO_PER_COMPASS=$(expr ${FREQ} / ${COMPASS})
OUTDIR=${self}/out
mkdir -p ${OUTDIR}
setopts

# Detect arm linux compiler directory
which arm-linux-gcc > /dev/null || export PATH=/usr/local/src/gumstix-buildroot/build_arm/staging_dir/bin:$PATH
which arm-linux-gcc > /dev/null || { echo "Cross compiler not found!"; }

case ${1} in
    checkmod) setopts; cpreprocess; checkmod ;;
    savemod) setopts; savemod ;;
#
    compile) setopts; cpreprocess; compile ;;
    run) run ;;
    bench) bench ;;
# Actions for gumstix
    compilegumstix) setopts; cpreprocess; compilegumstix ;;
    sendgumstix) sendgumstix ;;
    sndserver) sndserver ;;
    rungumstix) rungumstix ;;
    benchgumstix) benchgumstix ;;
    copysample) copysample ;;
    *) help ;;
esac

