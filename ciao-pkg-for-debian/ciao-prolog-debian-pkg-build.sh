#!/bin/bash

# set -x

echo "running $0 $* ... "

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ] || [ -z "$3" ] || [ "$3" == "" ] || [ -z "$4" ] || [ "$4" == "" ]; then
	echo " "
	echo "This is an utility to build Ciao Prolog debian packages."
	echo "usage: $0 DEST_FOLDER VERSION SVN_REVISION_CIAO SVN_REVISION_DEBIAN_CIAO_REPOS "
	echo "example: $0 ~/tmp 1.13 11293 "
	echo "example: $0 ~/tmp 1.14.2 13646 382"
	echo "example: $0 ~/tmp 1.15.0 14440 latest"
	echo "example: $0 ~/tmp 1.15.0 14854 826"
	echo "example: $0 ~/tmp 1.15.0 14944 922"
	echo "example: $0 ~/tmp 1.15.0 14990 969"
	echo "example: $0 ~/tmp 1.15.0 14992 969"
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
${SCRIPT_DIR}/ciao-prolog-apply-patches.sh ${DEST_FOLDER}/${FOLDER_NAME} do_not_apply

# FIXES.
# ${SCRIPT_DIR}/ciao-prolog-fixes.sh

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


