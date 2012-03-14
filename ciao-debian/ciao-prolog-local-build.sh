#!/bin/bash

# set -x

if [ -z "$1" ] || [ "$1" == "" ]; then
	echo " "
	echo "This is an utility to build Ciao Prolog debian packages."
	echo "usage: $0 SVN_REVISION_CIAO "
	echo "usage: $0 nocheckout "
	echo "example: $0 14440 "
	echo "example: $0 14482 "
	echo "example: $0 14505 <- YES (at least as pbuilder pkg)"
	echo "example: $0 14566 <- NO "
	echo "example: $0 14580 <- NO "
	echo " "
	exit 0
else
	REVISION="$1"
fi;

function echo_ten() {
    echo " "
    echo " "
    echo " " 
    echo " "
    echo " "
    echo " " 
    echo " "
    echo " "
    echo " " 
    echo " "
}

# Compilation script.
CIAOSETUP="./ciaosetup"

# Repositories urls.
REPOS_1=svn+ssh://clip.dia.fi.upm.es/home/clip/SvnReps/Systems/CiaoDE/trunk
# REPOS_2=https://babel.ls.fi.upm.es/svn/negation_and_fuzzy_logic/ciao-debian/
# SVNREPO_2=svn+ssh://clip.dia.fi.upm.es/home/egallego/clip/repos/ciao-debian/

FOLDER_NAME=~/secured/tests/CiaoDE_trunk

# Ensure folder exists and has a pristine copy.
# rm -fR $FOLDER_NAME
mkdir -p $FOLDER_NAME
pushd $FOLDER_NAME

# Update and export the Ciao's repository
echo " "
if [ -d .svn ]; then
	echo "updating CIAO to revision $REVISION from $REPOS_1"
	svn revert -R .
	if [ ! "$REVISION" == "nocheckout" ]; then
	    svn update --revision $REVISION
	fi
else
	echo "checking out CIAO to revision $REVISION from $REPOS_1"
	if [ ! "$REVISION" == "nocheckout" ]; then
	    svn co $REPOS_1 . --revision $REVISION
	fi
fi
echo " "

echo " "
${CIAOSETUP} clean
echo " "
${CIAOSETUP} clean_config
echo " "
${CIAOSETUP} realclean
echo " "
${CIAOSETUP} braveclean
echo " "

# exit 0
echo_ten

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
    --optimizing_compiler=yes \
    --use_threads=yes \
    --use_posix_locks=no \
    --and_parallel_execution=no \
    --par_back=no \
    --tabled_execution=no \
    --optim_level=optimized \
    --with_chr=yes \
    --with_ciaoppcl=no \
    --compress_lib=no \
    --unused_pred_warnings=no \
    --runtime_checks=no \
    --set_flag_options=yes 

echo " "
echo " "
echo " " 
#read -p "Press enter to continue with Ciao Prolog COMPILATION"
echo_ten

# Fixes
function nocompile_nor_distribute () {
    if [ ! -z "$1" ] && [ ! "$1" == "" ] && [ -d "$1" ]; then
	touch $1/NOCOMPILE
	pushd $1
	ls -1 *.pl > NOCOMPILE 2>&1
	popd
	touch $1/NODISTRIBUTE
    else
	echo "Erroneous folder: $1"
    fi
}

echo "FIXES:"
#nocompile_nor_distribute ciao/contrib/clpfd
#nocompile_nor_distribute ciao/contrib/difference_constraints
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
