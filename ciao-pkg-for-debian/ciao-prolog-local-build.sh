#!/bin/bash

# set -x

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ] || [ -z "$3" ] || [ "$3" == "" ] || [ -z "$4" ] || [ "$4" == "" ]; then
        echo " "
        echo "This is an utility to build Ciao Prolog debian packages."
        echo "usage: $0 DEST_FOLDER VERSION SVN_REVISION_CIAO SVN_REVISION_DEBIAN_CIAO_REPOS "
        echo "example: $0 ~/tmp 1.13 11293 "
        echo "example: $0 ~/tmp 1.14.2 13646 382"
        echo "example: $0 ~/tmp 1.15.0 14440 latest"
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
pushd ${DEST_FOLDER}/${FOLDER_NAME}

debian/ciao-prolog-configure-clean-build-and-install.sh

echo " "
popd
echo " "
