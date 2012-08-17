#!/bin/bash

echo "running $0 $* ... "

if [ -z $1 ] || [ "$1" == "" ] || [ -z $2 ] || [ "$2" == "" ] || [ -z $3 ] || [ "$3" == "" ]; then
	echo " "
	echo "ERROR: This arguments needs three arguments to be run."
	echo "${0} CURRENT_DIR PKG_FINAL_PATH PKG_FINAL_SUBPATH"
	echo " "
	exit -1
fi

CURRENT_DIR="${1}" 
PKG_FINAL_PATH="${2}" 
PKG_FINAL_SUBPATH="${3}"
FULL_PATH="${PKG_FINAL_PATH}/${PKG_FINAL_SUBPATH}/"
LOGS_PATH="${FULL_PATH}/debian-pkg-logs/"

# Info: PKG_FINAL_SUBPATH must point to ${CURRENT_DIR}/debian/tmp

function test_retval() {
    if [ -z $1 ] || [ "$1" == "" ] || [ ! "$1" == "0" ]; then
	echo " "
	echo "Return value is: --${1}-- "
	echo " "
	exit $1
    fi
}


# ensure folders exist.
mkdir -p ${PKG_FINAL_SUBPATH}
mkdir -p ${PKG_FINAL_PATH}
mkdir -p ${FULL_PATH}
mkdir -p ${LOGS_PATH}

./debian/ciao-prolog-check-folders-status.sh ${CURRENT_DIR} ${PKG_FINAL_PATH} ${PKG_FINAL_SUBPATH}

echo " "
echo "Removing subversion subfolders"
echo " "
find . -name ".svn" -type d -exec rm -vrf {} \;

./debian/ciao-prolog-check-folders-status.sh ${CURRENT_DIR} ${PKG_FINAL_PATH} ${PKG_FINAL_SUBPATH}

for file in *; do
	if [ ! -z $file ] && [ ! "$file" == "" ] && [ ! "$file" == "debian" ] && [ ! "$file" == ".svn" ]; then
		mv -v $file ${PKG_FINAL_SUBPATH} 2>&1 >> ${LOGS_PATH}/files-moved.txt
	fi
done
echo " " >> ${LOGS_PATH}/files-moved.txt
echo " " >> ${LOGS_PATH}/files-moved.txt
ls -la 2>&1 >> ${LOGS_PATH}/files-moved.txt
echo " " >> ${LOGS_PATH}/files-moved.txt
echo " " >> ${LOGS_PATH}/files-moved.txt

# To save configurations for emacs and bash
mkdir -p /root
touch /root/.emacs
touch /root/.bashrc

# Configure, clean, build and install.
./debian/ciao-prolog-configure-clean-build-and-install.sh ${PKG_FINAL_SUBPATH}
test_retval $?

# Need to move files to the $FULL_PATH
echo " "
echo "Saving user configuration files to a non-problematic path:  ${LOGS_PATH} "
mv -v ${PKG_FINAL_PATH}/root/.emacs ${LOGS_PATH}/add_to_.emacs
mv -v ${PKG_FINAL_PATH}/root/.bashrc ${LOGS_PATH}/add_to_.bashrc
echo " "
echo "Moving the sources and the compiled files from ${PKG_FINAL_SUBPATH} to ${FULL_PATH} "
for file in ${PKG_FINAL_SUBPATH}/*; do
	if [ ! -z $file ] && [ ! "$file" == "" ] && [ ! "$file" == "debian" ] && [ ! "$file" == ".svn" ]; then
		mv -v ${PKG_FINAL_SUBPATH}/${file} ${FULL_PATH} 2>&1 >> ${LOGS_PATH}/files-moved.txt
	fi
done
echo " " >> ${LOGS_PATH}/files-moved.txt
echo " " >> ${LOGS_PATH}/files-moved.txt
ls -la 2>&1 >> ${LOGS_PATH}/files-moved.txt
echo " " >> ${LOGS_PATH}/files-moved.txt
echo " " >> ${LOGS_PATH}/files-moved.txt

./debian/ciao-prolog-check-folders-status.sh ${CURRENT_DIR} ${PKG_FINAL_PATH} ${PKG_FINAL_SUBPATH}

