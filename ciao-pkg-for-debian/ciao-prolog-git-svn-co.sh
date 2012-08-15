#!/bin/bash

# set -x

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ] || [ -z "$3" ] || [ "$3" == "" ]; then
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

# Update and export the Ciao's repository
echo " "
if [ -d .git ]; then
    echo "Current status of git repository: "
    git status --untracked-files=no
    echo " "
    if [ "$RESET"=="reset" ]; then
        echo "return the entire working tree to the last committed state "
        echo "git reset --hard HEAD "
        git reset --hard HEAD
    else
        echo "Not returning to the last committed state (useful for testing)"
    fi
    echo " "
    echo "updating CIAO to revision $REVISION from $REPOS_1"
    # svn revert -R .
    if [ ! "$REVISION" == "nocheckout" ]; then
        git svn fetch --revision $REVISION
    fi
else
        echo "checking out CIAO to revision $REVISION from $REPOS_1"
        if [ ! "$REVISION" == "nocheckout" ]; then
            git svn clone $REPOS_1 . --revision $REVISION
        fi
fi
echo " "

popd
echo " "
popd

