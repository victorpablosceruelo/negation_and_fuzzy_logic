#!/bin/bash

echo "running $0 $* ... "

if [ -z $1 ] || [ "$1" == "" ] || [ -z $2 ] || [ "$2" == "" ]; then
	echo " "
	echo "ERROR: This arguments needs two arguments to be run."
	echo "${0} CHROOT_FOLDER DEST_FOLDER"
	echo " "
	exit -1
fi

CHROOT_FOLDER="$1" 
DEST_FOLDER="${2}"
FULL_PATH="${CHROOT_FOLDER}/${DEST_FOLDER}/"

# Info: DEST_FOLDER must point to $(CURDIR)/debian/tmp

# ensure folders exist.
mkdir -p ${CHROOT_FOLDER}
mkdir -p ${DEST_FOLDER}

for file in *; do
	if [ ! -z $file ] && [ ! "$file" == "" ] && [ ! "$file" == "debian" ] && [ ! "$file" == ".svn" ]; then
		mv -v $file ${FULL_PATH}
	fi
done

# Copy the scripts in debian folder.
cp -dpRv debian ${FULL_PATH}

pushd ${FULL_PATH}
ln -s debian/ciao-prolog-build-and-install.sh ciao-prolog-build-and-install.sh
popd

# To save configurations for emacs and bash
mkdir -p ${CHROOT_FOLDER}/root
touch ${CHROOT_FOLDER}/root/.emacs
touch ${CHROOT_FOLDER}/root/.bashrc

/usr/sbin/chroot ${CHROOT_FOLDER} ${DEST_FOLDER}/ciao-prolog-build-and-install.sh

mv ${CHROOT_FOLDER}/root/.emacs ${FULL_PATH}/add_to_.emacs
mv ${CHROOT_FOLDER}/root/.bashrc ${FULL_PATH}/add_to_.bashrc


