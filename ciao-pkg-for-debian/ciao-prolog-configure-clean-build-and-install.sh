#!/bin/bash

echo "running $0 $* ... "

if [ -z $1 ] || [ "$1" == "" ]; then
	echo "${0}: usage: $0 COMPILATION_PATH"
fi

COMPILATION_PATH="$1"

mkdir -p ${COMPILATION_PATH}
if [ ! -d ${COMPILATION_PATH} ]; then
    exit -1
fi

function test_retval() {
    if [ -z $1 ] || [ "$1" == "" ] || [ ! "$1" == "0" ]; then
	echo " "
	echo "Return value is: --${1}-- "
	echo " "
	exit $1
    fi
}

pushd ${COMPILATION_PATH}
read -t 60 -p "Press enter to continue with Ciao Prolog INSTALLATION"

CIAOSETUP=ciaosetup

echo " "
echo " "
./${CIAOSETUP} clean
test_retval $?
echo " "
echo " "
./${CIAOSETUP} clean_config
test_retval $?
echo " "
echo " "
./${CIAOSETUP} realclean
test_retval $?
echo " "
echo " "
./${CIAOSETUP} braveclean
test_retval $?
echo " "
echo " "

#######################################################
#######################################################
#######################################################

#    --registration_type=all \
#    --instype=global \
#    --prefix=/usr \
#    --installgroup=root \
#    --update_bashrc=no \
#    --optim_level=normal \
#    --prefix=~/secured/local \
#    --htmldir=~/public_html/ciao-manual \
#    --htmlurl=http://localhost/ciao-manual \
#    --docdir=~/secured/local/share/doc/ciao-prolog \
#    --mandir=~/secured/local/share/man \
#    --infodir=~/secured/local/share/info \
#    --web_images_path=~/secured/local/share/doc/ciao-prolog/html \
#    --with_ciaoppcl=no \

# 	--update_bashrc=no \
#	--update_cshrc=no \
#	--update_dotemacs=no \
#	--docdir=/usr/share/doc/ciao-prolog \
#	--mandir=/usr/share/man \
#	--infodir=/usr/share/info \
#	--web_images_path=/usr/share/doc/ciao-prolog/html \
#	--htmldir=/usr/share/doc/ciao-prolog/html \
#	--installgroup=root \


./ciaosetup configure \
    --stop-if-error=yes \
    --registration_type=user \
    --instype=local \
    --execmode=755 \
    --datamode=644 \
    --update_bashrc=yes \
    --dotbashrc=~/.bashrc \
    --update_cshrc=no \
    --install_prolog_name=no \
    --install_emacs_support=yes \
    --install_xemacs_support=no \
    --update_dotemacs=yes \
    --dotemacs=~/.emacs \
    --emacsinitdir=~/.emacs.d \
    --update_dotxemacs=no \
    --with_mysql=yes \
    --mysql_client_directory=/usr/lib \
    --with_gsl=no --with_ppl=no \
    --with_java_interface=yes \
    --with_ant=yes \
    --optimizing_compiler=no \
    --use_threads=yes \
    --use_posix_locks=no \
    --and_parallel_execution=no \
    --par_back=no \
    --tabled_execution=no \
    --optim_level=normal \
    --with_chr=no \
    --compress_lib=no \
    --unused_pred_warnings=yes \
    --runtime_checks=no \
    --set_flag_options=yes 

test_retval $?

	# New in version 1.15.0 r14285
#	./${CIAOSETUP} configure 
#	./${CIAOSETUP} configure --setup
#	./${CIAOSETUP} build
#	./${CIAOSETUP} docs
#	./${CIAOSETUP} install
#	./${CIAOSETUP} install_to_destroot --destdir=[value]

# Add here commands to compile and install the package.
# ./${CIAOSETUP} build
# ./${CIAOSETUP} docs
# ./${CIAOSETUP} install_to_destroot --destdir=${CURDIR}/debian/tmp
# ./${CIAOSETUP} install_to_destroot --destdir=/usr

#######################################################
#######################################################
#######################################################

echo " "
echo " "

# Add here commands to compile the package.
./${CIAOSETUP} build
test_retval $?
echo " "
echo " "
./${CIAOSETUP} docs
test_retval $?
echo " "
echo " "

read -t 60 -p "Press enter to continue with Ciao Prolog INSTALLATION"
echo " "
echo " "

# And the commands to install it.
./${CIAOSETUP} install
test_retval $?
echo " "
echo " "

popd

# EOF
