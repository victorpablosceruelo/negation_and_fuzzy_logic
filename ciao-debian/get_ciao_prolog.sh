#!/bin/bash

# set -x

if [ -z $1 ] || [ "$1" == "" ]; then
	echo "This is an utility to build Ciao Prolog debian packages."
	echo "Please enter Ciao Prolog subversion revision to download."
	echo "We recommend you to use revision 11293"
	exit 0
else
	REVISION="$1"
fi;

# Repositories urls.
REPOS_1=svn+ssh://clip.dia.fi.upm.es/home/clip/SvnReps/Systems/CiaoDE/trunk
REPOS_2=https://babel.ls.fi.upm.es/svn/negation_and_fuzzy_logic/ciao-debian/
# SVNREPO_2=svn+ssh://clip.dia.fi.upm.es/home/egallego/clip/repos/ciao-debian/

DATE=`date +%Y%m%d`
# REVISION=11293
VERSION=1.13+r$REVISION
FOLDER_NAME=ciao-prolog-$VERSION
FILE_NAME=ciao-prolog_$VERSION
BUILD_TGZ=$FILE_NAME.orig.tar.gz
BUILD_DSC=$FILE_NAME.dsc
BUILD_DIFF=$FILE_NAME.diff
BUILD_DIFF_GZ=$FILE_NAME.diff.gz

# Where to put everything.
pushd ~/tmp

# Clean up
rm -fv $BUILD_TGZ $BUILD_DSC $BUILD_DIFF $BUILD_DIFF_GZ

# Ensure folder exists.
# rm -fR $FOLDER_NAME
mkdir -p $FOLDER_NAME
pushd $FOLDER_NAME

# Update and export the Ciao's repository
echo " "
if [ -d .svn ]; then
	echo "updating $REPOS_1 to revision $REVISION"
	svn update --revision $REVISION
else
	echo "checking out $REPOS_1 to revision $REVISION"
	svn co $REPOS_1 . --revision $REVISION
fi
popd
echo " "

# Ensure folder exists.
# rm -fR $FOLDER_NAME/debian
mkdir -p $FOLDER_NAME/debian
pushd $FOLDER_NAME/debian

echo " "
# if [ -d .svn ]; then
if [ -d .git ]; then
	echo "updating $REPOS_2 to last revision "
	# svn update
	git svn rebase
else
	echo "checking out $REPOS_2 to last revision."
	# svn co $REPOS_2 . 
	git svn clone $REPOS_2 .
fi
popd
echo " "

# Create .orig tarball
tar cvfz $BUILD_TGZ $FOLDER_NAME

# Create a suitable changelog entry.
pushd $FOLDER_NAME
dch -b -v $VERSION "Snapshot version $VERSION date $DATE."
popd
dpkg-source -b $FOLDER_NAME

echo " "
echo " "
echo "-> Now run: [ the first one only if you did not do it before ]"
echo " sudo cowbuilder --create "
echo " sudo cowbuilder --update "
echo " sudo cowbuilder --build $BUILD_DSC "
echo " "

popd
echo " "
echo " "

