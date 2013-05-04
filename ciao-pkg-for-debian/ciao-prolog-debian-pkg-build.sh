#!/bin/bash

# set -x

echo "running $0 $* ... "

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ]; then
	echo " "
	echo "This is an utility to build Ciao Prolog debian packages."
	echo "usage: $0 SOURCE_FOLDER VERSION"
	echo "example: $0 ~/tmp 1.15.0+r14992"
	echo "example: $0 ~/tmp 1.15.0+r14995"
	echo "example: $0 ~/tmp 1.15.0+r17121"
	echo " "
	exit 0
else
	SOURCE_FOLDER="$1"
	VERSION="$2"
fi;

DATE=`date +%Y%m%d`
FILE_NAME=ciao-prolog_${VERSION}
BUILD_TGZ=${FILE_NAME}.orig.tar.gz
BUILD_DSC=${FILE_NAME}.dsc
BUILD_DIFF=${FILE_NAME}.diff
BUILD_DIFF_GZ=${FILE_NAME}.diff.gz
SCRIPT_DIR=`dirname $0`

# Apply patches.
${SCRIPT_DIR}/ciao-prolog-apply-patches.sh ${SOURCE_FOLDER} do_not_apply

# FIXES.
${SCRIPT_DIR}/ciao-prolog-fixes.sh

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


