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
	echo " "
	exit 0
else
	DEST_FOLDER="$1"
	VERSION="$2"
	REVISION="$3"
	DEBIAN_REPOS_REVISION="$4"
fi;

echo "$*"

# Repositories urls.
REPOS_1=svn+ssh://clip.dia.fi.upm.es/home/clip/SvnReps/Systems/CiaoDE/trunk
REPOS_2=https://babel.ls.fi.upm.es/svn/negation_and_fuzzy_logic/ciao-pkg-for-debian/
# SVNREPO_2=svn+ssh://clip.dia.fi.upm.es/home/egallego/clip/repos/ciao-debian/

DATE=`date +%Y%m%d`
PKG_VERSION=$VERSION+r$REVISION
FOLDER_NAME=ciao-prolog-$PKG_VERSION
FILE_NAME=ciao-prolog_$PKG_VERSION
BUILD_TGZ=$FILE_NAME.orig.tar.gz
BUILD_DSC=$FILE_NAME.dsc
BUILD_DIFF=$FILE_NAME.diff
BUILD_DIFF_GZ=$FILE_NAME.diff.gz

# Where to put everything.
mkdir -p ${DEST_FOLDER}
pushd ${DEST_FOLDER}

# Ensure folder exists and has a pristine copy.
mkdir -p $FOLDER_NAME
pushd $FOLDER_NAME

if [ "$REVISION" != "latest" ]; then
    REVISION="--revision $REVISION"
else
    REVISION=""
fi

# Update and export the Ciao's repository
echo " "
echo "updating/checking out CIAO from $REPOS_1 ($REVISION)"
if [ -d .svn ]; then
	svn revert -R .
	svn update $REVISION
else
	svn co $REPOS_1 . $REVISION
fi
echo -n "Revision: "
svn info -R | grep "Revision\: " | sort -k2nr | head -n1 | awk -F"Revision: " '{print $2}'

popd
echo " "

# Ensure folder exists.
mkdir -p $FOLDER_NAME/debian
pushd $FOLDER_NAME/debian

if [ "$DEBIAN_REPOS_REVISION" == "latest" ]; then
    DEBIAN_REPOS_REVISION="--revision $DEBIAN_REPOS_REVISION"
else
    DEBIAN_REPOS_REVISION=""
fi

echo " "
echo "updating debian subfolder from $REPOS_2 ($DEBIAN_REPOS_REVISION)"
if [ -d .svn ]; then
	svn revert -R .
	svn update  $DEBIAN_REPOS_REVISION
else
	svn co $REPOS_2 . --revision $DEBIAN_REPOS_REVISION
fi
echo -n "Revision: "
svn info -R | grep "Revision\: " | sort -k2nr | head -n1 | awk -F"Revision: " '{print $2}'

popd
echo " "
popd

