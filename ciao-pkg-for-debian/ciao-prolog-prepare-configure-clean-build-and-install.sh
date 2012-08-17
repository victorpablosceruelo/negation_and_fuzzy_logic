#!/bin/bash

echo "running $0 $* ... "

if [ -z $1 ] || [ "$1" == "" ] || [ -z $2 ] || [ "$2" == "" ]; then
	echo " "
	echo "ERROR: This arguments needs two arguments to be run."
	echo "${0} PKG_FINAL_PATH PKG_FINAL_SUBPATH"
	echo " "
	exit -1
fi

PKG_FINAL_PATH="$1" 
PKG_FINAL_SUBPATH="${2}"
FULL_PATH="${PKG_FINAL_PATH}/${PKG_FINAL_SUBPATH}/"
LOGS_PATH="${FULL_PATH}/debian-pkg-logs/"

# Info: PKG_FINAL_SUBPATH must point to $(CURDIR)/debian/tmp

# ensure folders exist.

mkdir -p ${PKG_FINAL_SUBPATH}
mkdir -p ${PKG_FINAL_PATH}
mkdir -p ${FULL_PATH}
mkdir -p ${FULL_PATH}/debian_pkg

echo "Listing the files in $(CURDIR) before in $(LOGS_PATH)/files-in-curdir-before.txt."
echo "Files in $(CURDIR) before." > $(LOGS_PATH)/files-in-curdir-before.txt
find $(CURDIR) >> $(LOGS_PATH)/files-in-curdir-before.txt
echo "Done. "
echo " "
echo " "
echo " "


for file in *; do
	if [ ! -z $file ] && [ ! "$file" == "" ] && [ ! "$file" == "debian" ] && [ ! "$file" == ".svn" ]; then
		mv -v $file ${PKG_FINAL_SUBPATH} 2>&1 >> $(LOGS_PATH)/files-moved.txt
	fi
done

# To save configurations for emacs and bash
mkdir -p /root
touch /root/.emacs
touch /root/.bashrc

pushd ${PKG_FINAL_SUBPATH}
debian/ciao-prolog-configure-clean-build-and-install.sh ${PKG_FINAL_SUBPATH}
popd

# Need to move files to the $FULL_PATH
echo " "
echo "Saving user configuration files to a non-problematic path:  $(LOGS_PATH) "
mv -v ${PKG_FINAL_PATH}/root/.emacs $(LOGS_PATH)/add_to_.emacs
mv -v ${PKG_FINAL_PATH}/root/.bashrc $(LOGS_PATH)/add_to_.bashrc
echo " "
echo "Moving the sources and the compiled files from ${PKG_FINAL_SUBPATH} to ${FULL_PATH} "
mv -v ${PKG_FINAL_SUBPATH} ${FULL_PATH}

echo " "
echo " "
echo " "
echo "Listing the files in $(CURDIR) after in $(LOGS_PATH)/files-in-curdir-after.txt."
echo "Files in $(CURDIR) after." > $(LOGS_PATH)/files-in-curdir-after.txt
find $(CURDIR) >> $(LOGS_PATH)/files-in-curdir-after.txt

