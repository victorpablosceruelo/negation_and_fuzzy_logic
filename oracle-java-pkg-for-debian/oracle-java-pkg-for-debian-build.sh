#!/bin/bash

# set -x

echo "running $0 $* ... "

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ]; then
	echo " "
	echo "This is an utility to build Ciao Prolog debian packages."
	echo "usage: $0 ORIG_FOLDER DEST_FOLDER  "
	echo "example: $0 . ~/tmp "
	echo " "
	exit 0
else
	ORIG_FOLDER="$1"
	DEST_FOLDER="$2"
fi;

DATE=`date +%Y%m%d`
PKG_VERSION="1.0.3"
FOLDER_NAME="oracle-java-pkg-for-debian-${PKG_VERSION}"
FILE_NAME=oracle-java-pkg-for-debian_${PKG_VERSION}
BUILD_TGZ=${FILE_NAME}.orig.tar.gz
BUILD_DSC=${FILE_NAME}.dsc
BUILD_DIFF=${FILE_NAME}.diff
BUILD_DIFF_GZ=${FILE_NAME}.diff.gz
SCRIPT_DIR=`dirname $0`

mkdir -p ${DEST_FOLDER}
mkdir -p ${DEST_FOLDER}/${FOLDER_NAME}
mkdir -p ${DEST_FOLDER}/${FOLDER_NAME}/debian
cp ${ORIG_FOLDER}/* ${DEST_FOLDER}/${FOLDER_NAME}/debian

# Ensure we work locally.
pushd ${DEST_FOLDER}

# Clean up
rm -fv $BUILD_TGZ $BUILD_DSC $BUILD_DIFF $BUILD_DIFF_GZ

# Create .orig tarball
echo " -> Compressing $FOLDER_NAME in file $BUILD_TGZ "
tar cfz $BUILD_TGZ $FOLDER_NAME

# Create a suitable changelog entry.
pushd $FOLDER_NAME
dch -b -v $PKG_VERSION "Snapshot version $PKG_VERSION date $DATE."
popd
dpkg-source -b $FOLDER_NAME

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
echo " sudo cowbuilder --build ${DEST_FOLDER}/$BUILD_DSC "
echo " "

echo " "
echo " "

set -x
sudo cowbuilder --build ${DEST_FOLDER}/${BUILD_DSC}
set +x
popd

echo "-> Now run "
echo " sudo ${SCRIPT_DIR}/ciao-prolog-autoinstall.sh "
echo " "


