#!/bin/bash

# set -x

echo "running $0 $* ... "

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ] || [ -z "$3" ] || [ "$3" == "" ]; then
	echo " "
	echo "This is an utility to build Ciao Prolog debian packages."
	echo "usage: $0 BASE_FOLDER SOURCE_FOLDER VERSION"
	echo "example: $0 ~/tmp CiaoDE-1.15-1712-ga8c821b 1.15.0"
	echo "result: a debian package with version 1.15.0+r{current_date}"
	echo " "
	exit 0
else
    BASE_FOLDER="$1"
    SOURCE_FOLDER="$2"
    DISTRO_VERSION="$3"
fi;

DATE=`date +%Y%m%d.%Hh`
VERSION=${DISTRO_VERSION}+r${DATE}
FILE_NAME=ciao-prolog_${VERSION}
BUILD_TGZ=${FILE_NAME}.orig.tar.gz
BUILD_DSC=${FILE_NAME}.dsc
BUILD_DIFF=${FILE_NAME}.diff
BUILD_DIFF_GZ=${FILE_NAME}.diff.gz
SCRIPT_DIR=`dirname $0`

# PBUILDER_KEYRING_OPTS=--keyring=/usr/share/keyrings/ubuntu-archive-keyring.gpg
PBUILDER_KEYRING_OPTS=--keyring=/etc/apt/trusted.gpg

# Apply patches.
# ${SCRIPT_DIR}/ciao-prolog-debian-pkg-build-apply-patches.sh ${BASE_FOLDER}/${SOURCE_FOLDER} do_not_apply

# FIXES.
${SCRIPT_DIR}/ciao-prolog-debian-pkg-build-fixes.sh ${BASE_FOLDER}/${SOURCE_FOLDER}

if [ ! -f changelog ] || [ ! -f compat ] || [ ! -f control ]; then 
    echo "This script must be run from the folder containing changelog, compat and control files. "
    exit 0
fi

# Copy all the mgmt files to the debian subfolder.
mkdir -p ${BASE_FOLDER}/${SOURCE_FOLDER}/debian/
cp -dpR * ${BASE_FOLDER}/${SOURCE_FOLDER}/debian/

# Ensure we work locally.
pushd ${BASE_FOLDER}

# Clean up
rm -fv $BUILD_TGZ $BUILD_DSC $BUILD_DIFF $BUILD_DIFF_GZ

# Create .orig tarball
echo " -> Compressing ${SOURCE_FOLDER} in file $BUILD_TGZ "
tar cfz $BUILD_TGZ ${SOURCE_FOLDER}

# Create a suitable changelog entry.
pushd ${SOURCE_FOLDER}
dch -b -v $VERSION "Snapshot version $PKG_VERSION date $DATE."
popd
dpkg-source -b ${SOURCE_FOLDER}

echo " "
echo "-> It is needed for the following operation the symbolic links: "
echo "   /etc/pbuilderrc -> pbuilderrc "
echo "   /etc/pbuilder/pbuilderrc -> pbuilderrc "
# echo "   /etc/pbuilder/pbuilder-apt -> pbuilder-apt "
echo " "
echo "-> Now run: [ the first two only if you did not execute them before ]"
echo " sudo pbuilder --clean "
echo " sudo cowbuilder --create "
echo " sudo cowbuilder --update "
echo " sudo cowbuilder --build ${BASE_FOLDER}/${BUILD_DSC} "
echo " "

echo " "
echo " "

set -x
sudo cowbuilder --build ${BASE_FOLDER}/${BUILD_DSC} ${KEYRING_OPTS}
set +x
popd

echo "-> Now run "
echo " sudo ${SCRIPT_DIR}/ciao-prolog-autoinstall.sh "
echo " "


