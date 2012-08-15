#!/bin/bash

# set -x

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ] || [ -z "$3" ] || [ "$3" == "" ]; then
        echo " "
        echo "This is an utility to build Ciao Prolog debian packages."
        echo "usage: $0 DEST_FOLDER VERSION SVN_REVISION_CIAO SVN_REVISION_DEBIAN_CIAO_REPOS "
        echo "example: $0 ./tmp 1.13 11293 "
        echo "example: $0 ./tmp 1.14.2 13646 382"
        echo "example: $0 ./tmp 1.15.0 14440 latest"
        echo " "
        exit 0
else
        DEST_FOLDER="$1"
        VERSION="$2"
        SVN_REVISION_CIAO="$3"
        SVN_REVISION_DEBIAN_CIAO_REPOS="$4"
fi;

DATE=`date +%Y%m%d`
PKG_VERSION=${VERSION}+r${SVN_REVISION_CIAO}
FOLDER_NAME=ciao-prolog-${PKG_VERSION}
FILE_NAME=ciao-prolog_${PKG_VERSION}
BUILD_TGZ=${FILE_NAME}.orig.tar.gz
BUILD_DSC=${FILE_NAME}.dsc
BUILD_DIFF=${FILE_NAME}.diff
BUILD_DIFF_GZ=${FILE_NAME}.diff.gz
SCRIPT_DIR=`dirname $0`

# Checkout the correct revisions.
${SCRIPT_DIR}/ciao-prolog-svn-co.sh ${DEST_FOLDER} ${VERSION} ${SVN_REVISION_CIAO} ${SVN_REVISION_DEBIAN_CIAO_REPOS}

# Apply patches.
${SCRIPT_DIR}/ciao-prolog-apply-patches.sh ${DEST_FOLDER}/${FOLDER_NAME}

# Ensure we work locally.
pushd ${DEST_FOLDER}

# Compilation script.
CIAOSETUP="./ciaosetup"
echo " "
${CIAOSETUP} clean
echo " "
${CIAOSETUP} clean_config
echo " "
${CIAOSETUP} realclean
echo " "
${CIAOSETUP} braveclean
echo " "


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


${CIAOSETUP} configure \
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

echo " "
echo " "
echo " " 
#read -p "Press enter to continue with Ciao Prolog COMPILATION"
echo_ten

# Fixes
echo "FIXES:"
nocompile_nor_distribute ciao/contrib/clpfd
nocompile_nor_distribute ciao/contrib/difference_constraints
nocompile_nor_distribute ciao/contrib/ppl/0_10
nocompile_nor_distribute ciao/contrib/ppl/0_9
rm -fv ciao/contrib/cneg/NOCOMPILE ciao/contrib/cneg/NODISTRIBUTE

echo_ten

./${CIAOSETUP} build
echo_ten
./${CIAOSETUP} docs
echo_ten
read -p "Press enter to continue with Ciao Prolog INSTALLATION"
echo_ten
./${CIAOSETUP} install

echo " "
popd
echo " "
