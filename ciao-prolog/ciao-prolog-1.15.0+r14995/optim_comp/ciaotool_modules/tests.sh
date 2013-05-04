#!/bin/bash
# Automated Tests (for Ciao developers)
# Author: Jose F. Morales

# Obtain the directory where this script is located
old_dir=`pwd`; cd `dirname $0`; self=`pwd`; cd ${old_dir}; old_dir=

# ---------------------------------------------------------------------------
# Imports

ocroot="${self}/.."
source ${ocroot}/ciaotool_modules/compat.sh
source ${ocroot}/ciaotool_modules/config.sh
source ${ocroot}/ciaotool_modules/archdump.sh
source ${ocroot}/ciaotool_modules/car.sh

# ---------------------------------------------------------------------------

trap 'exit -1' SIGINT

# ---------------------------------------------------------------------------

fulltests() {
    testcomp
    testinccomp
    ${ocroot}/ciaotool update-all || return 0
    ${ocroot}/ciaotool update-comp-js || return 0 # for testing JS backend
    do_tests runexec runexec
    do_common check
}

testinccomp() {
    ${ocroot}/ciaotool --stats inc || return 0
}

testcomp() {
    ${ocroot}/ciaotool --stats update-comp || return 0
    ${ocroot}/ciaotool bench briefcompareemu
}

do_common() {
    local ACTION=$1
    case ${ACTION} in
	check) ;;
	eval) ;;
	*) ${ocroot}/ciaotool bench ${ACTION}emu ;;
    esac
    do_tests checkexec ${ACTION}exec
    do_tests checkmod ${ACTION}mod
    do_tests mtsys mtsys-${ACTION}mod-ciao2
    do_tests mtsys mtsys-${ACTION}mod-ciao3
    # benchmarks for the shoot-out game
    ${ocroot}/testsuite/shootout-c/doall.sh ${ACTION}-all
    # benchmarks for the Javascript backend
    if [ x"${ACTION}" == x"eval" ]; then
	echo "(The Javascript backend is evaluated by hand)"
    else
	${ocroot}/ciaotool js-backend ${ACTION}-all
    fi
}

# ---------------------------------------------------------------------------
# Miscellaneous routines
# TODO: share with ciaotool

# Dump emulator loop size for ciaoloader_testing

VERNAME="testing"
versuf="_${VERNAME}"

loader_testing="${cache_dir}/bin/ciaoloader${versuf}"

emusize() {
    objdump -d ${loader_testing}.car/arch | funcsize${versuf} | grep wam__2
}

# Dump native code and emulator loop size for a given executable
exe_info() {
    echo all native code size: `stat ${1}.car/arch -c "%s"` bytes
    echo emulator loop size: `objdump -d ${1}.car/arch | funcsize${versuf} | grep wam__2`
}

# Rewrite input using Prolog comments 
comment() {
    while read x; do echo "% ${x}"; done
}

# ---------------------------------------------------------------------------
# Perform a set of tests

# test ${1} has property ${2}
has_prop() {
    local i
    local what
    local props
    what=${1}
    props=${2}
    test -r ${props} || return 1
    for i in `cat ${props}`; do
	if [ ${i} == ${what} ]; then
	    return 0
	fi
    done
    return 1
}

# for each test in the tests directory:
#     do action ${2} if the tests has property ${1}
do_tests() {
    local i
    local what
    local action
    what=$1
    action=$2
    pushd ${ocroot}/testsuite/tests > /dev/null
    for i in *; do
	test -d $i || continue
	has_prop ${what} ${i}/PROPS || continue
	pushd ${i} > /dev/null
	${ocroot}/ciaotool bench ${action} ${i}
	popd > /dev/null
    done
    popd > /dev/null
}

# ---------------------------------------------------------------------------
# Abstract machine minimization tests

sabsmach_min_test() {
    local prg
    prg=$1
    [ -z ${prg} ] && return 1
    cache_dir=${ocroot}/testsuite/sabsmach_min/cache
    mkdir -p ${cache_dir}

    echo "Compiling under alternative cache using dead info (static exec)"
    ${ocroot}/ciaotool comp-testing --bootstrap ${prg}-stat ${prg}
    ${prg}-stat.car/clean
    ${prg}-stat.car/compile_native
    exe_info ${prg}-stat
    ciaodump --file dectok ${prg}-stat.car/noarch > ${prg}-stat.dead
    head -1 ${prg}-stat.dead # Print bytecode size
#
    ${ocroot}/ciaotool --cache-dir ${cache_dir} clean-cache
    ${ocroot}/ciaotool --cache-dir ${cache_dir} comp-testing --dead ${prg}-stat.dead --bootstrap ${prg}-small ${prg}
    ${prg}-small.car/clean
    ${prg}-small.car/compile_native
    exe_info ${prg}-small
    echo "Compiling under alternative cache using dead info (dynamic exec)"
    echo "(note: will not run unless a full absmach executes the external modules)"
    ${ocroot}/ciaotool comp-testing --dynexec ${prg}-dyn ${prg}
    ciaodump --file dectok ${prg}-dyn > ${prg}-dyn.dead
    head -1 ${prg}-dyn.dead # Print bytecode size
#
    ${ocroot}/ciaotool --cache-dir ${cache_dir} clean-cache
    ${ocroot}/ciaotool --cache-dir ${cache_dir} comp-testing --dead ${prg}-dyn.dead --bootstrap ${prg}-tiny ${prg}
    ${prg}-tiny.car/clean
    ${prg}-tiny.car/compile_native
    exe_info ${prg}-tiny
}

# ---------------------------------------------------------------------------

sabsmach_min_clean() {
    [ -r ${1}-dyn ] && rm -f ${1}-dyn
    delete_exe ${1}-stat
    delete_exe ${1}-small
    delete_exe ${1}-tiny
    rm -f ${1}-stat.dead
    rm -f ${1}-dyn.dead
}

# ---------------------------------------------------------------------------

sabsmach_min__all_compile() {
    for i in ${all}; do
	echo "======= COMPILING $i ======="
	sabsmach_min_test ${ocroot}/testsuite/sabsmach_min/${i}
    done
}

sabsmach_min__all_compile_and_clean() {
#all="hello/hello comp/comp"
    all="hello/hello comp/comp stream2/stream_dynamic_prolog stream2/stream_layered_prolog stream2/stream_naive_prolog"
#all=$(for i in oc/*.pl; do echo oc/$(basename $i .pl); done)
    for i in ${all}; do
	echo "======= COMPILING $i ======="
	sabsmach_min_test ${ocroot}/testsuite/sabsmach_min/${i}
	echo "======= CLEANING $i ======="
	sabsmach_min_clean ${ocroot}/testsuite/sabsmach_min/${i}
    done
}

sabsmach_min__all_clean() {
    rm -rf cache
    for i in ${all}; do
	echo "======= CLEANING $i ======="
	sabsmach_min_clean ${ocroot}/testsuite/sabsmach_min/${i}
    done
}

sabsmach_min__help() {
cat <<EOF
Usage: $(basename $0) [all-clean|all-compile|all-compile-and-clean]
EOF
}

sabsmach_min() {
    case $1 in
	all-clean) sabsmach_min__all_clean ;;
	all-compile) sabsmach_min__all_compile ;;
	all-compile-and-clean) sabsmach_min__all_compile_and_clean ;;
	*) sabsmach_min__help
    esac
}

# ---------------------------------------------------------------------------
# Abstract machine versions tests

sabsmach_vers__prepare_test() {
    OPTGRP=${1}
    OPTS=${2}

    echo "optgrp: ${OPTGRP}" >> ${outfile}
    echo "opt: ${OPTS}" >> ${outfile}

    cat <<EOF
---------------------------------------------------------------------------
Preparing test (options group: ${OPTGRP}, options: ${OPTS}) (date: `date`)
---------------------------------------------------------------------------
EOF
    cat "BUG: testing compiler and engine are not being used in evalmod, etc.! Add a flag to do eval with the testing system."
    # Regenerate the compiler and tools
    ABSMACH_OPTGRP=${OPTGRP} ABSMACH_OPTS=${OPTS} ${ocroot}/ciaotool clean-cache
    ABSMACH_OPTGRP=${OPTGRP} ABSMACH_OPTS=${OPTS} ${ocroot}/ciaotool update-comp-testing
    # Compile the ciaodump for testing
    # Compile the ciaoloader for testing
    ABSMACH_OPTGRP=${OPTGRP} ABSMACH_OPTS=${OPTS} ${ocroot}/ciaotool --stats update-loader-testing && \
    ABSMACH_OPTGRP=${OPTGRP} ABSMACH_OPTS=${OPTS} ${ocroot}/ciaotool update-apps-testing
}

sabsmach_vers__do_test() {
    OPTGRP=${1}
    OPTS=${2}
    cat <<EOF
---------------------------------------------------------------------------
Running test:  (options group: ${OPTGRP}, options: ${OPTS}) (date: `date`)
---------------------------------------------------------------------------
EOF
    ABSMACH_OPTGRP=${OPTGRP} ABSMACH_OPTS=${OPTS} emusize >> ${outfile}
    for t in 1 2 3; do
	echo "Try ${t}"
	echo "try: ${t}" >> ${outfile}
	ABSMACH_OPTGRP=${OPTGRP} ABSMACH_OPTS=${OPTS} do_tests evalexec evalexec >> ${outfile}
    done
}

GENERALOPTS=63
TAGOPTS=48

sabsmach_vers__help() {
    cat <<EOF
Usage: `basename $0` ACTION

Where ACTION is one of:

  clean                 Clean the results file
  summary [...]         Show summary of the results file
  all OPTGRP            Test all options for the option group OPTGRP
  single OPTGRP OPTS    Test the options OPTS for the option group OPTGRP

Values for OPTGRP:
   
  general               General absmach generation options (0-${GENERALOPTS})
  tags                  Tag scheme options (1-${TAGOPTS})
EOF
}

# Enumerate all options for a given option group
sabsmach_vers__opts() {
    OPTGRP=${1}
    case ${OPTGRP} in
	general)
	    n=0
            while [ ${n} -le ${GENERALOPTS} ]; do
	        echo ${n}
		n=$((${n}+1))
	    done
	    ;;
	tags)
	    n=1
            while [ ${n} -le ${TAGOPTS} ]; do
	        echo "tagscheme${n}"
		n=$((${n}+1))
	    done
	    ;;
    esac
}

sabsmach_vers__all() {
    OPTGRP=${1}
    for i in `sabsmach_vers__opts ${OPTGRP}`; do
	OPT="${i}"
	sabsmach_vers__prepare_test ${OPTGRP} ${OPT} && sabsmach_vers__do_test ${OPTGRP} ${OPT}
    done
}

sabsmach_vers__header() {
    system_info | comment >> ${outfile}
}

sabsmach_vers__summary() {
    # TODO: port python prototype to Prolog
    python2.4 ${self}/summary.py $*
}

sabsmach_vers() {
    ensure_regression_dir
    outdir="${regression_dir}/sabsmach_vers/out"
    outfile="${outdir}/rep.txt"
    mkdir -p ${outdir}

    cmd=${1}
    shift
    case ${cmd} in
	clean) rm ${outfile} ;;
	summary) sabsmach_vers__summary $* ;;
	all) sabsmach_vers__header && sabsmach_vers__all $* ;;
	single) sabsmach_vers__header && sabsmach_vers__prepare_test $* && sabsmach_vers__do_test $* ;;
	*) sabsmach_vers__help
    esac
}

# ---------------------------------------------------------------------------

action=$1
shift

do_help() {
    cat <<EOF
(see docs/tests.lpdoc for help)
EOF
}

case ${action} in
    full) fulltests ;;
    comp) testcomp ;;
    inccomp) testinccomp ;;
#
    runexecs) do_tests runexec runexec ;;
    check) do_common check $* ;;
#
    eval) do_common eval $* ;;
#
    briefcompare) do_common briefcompare $* ;;
    compare) do_common compare $* ;;
    save) do_common save $* ;;
#
    sabsmach-min)      sabsmach_min $* ;;
    sabsmach-vers)     sabsmach_vers $* ;;
    *)                 do_help; exit -1 ;;
esac

