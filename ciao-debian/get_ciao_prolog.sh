#!/bin/bash

# set -x

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ] || [ -z "$3" ] || [ "$3" == "" ]; then
	echo " "
	echo "This is an utility to build Ciao Prolog debian packages."
	echo "usage: $0 VERSION SVN_REVISION_CIAO SVN_REVISION_DEBIAN_CIAO_REPOS "
	echo "example: $0 1.13 11293 "
	echo "example: $0 1.14.2 13646 382"
	echo "example: $0 1.15.0 14440 latest"
	echo " "
	exit 0
else
	VERSION="$1"
	REVISION="$2"
	DEBIAN_REPOS_REVISION="$3"
fi;

# Repositories urls.
REPOS_1=svn+ssh://clip.dia.fi.upm.es/home/clip/SvnReps/Systems/CiaoDE/trunk
REPOS_2=https://babel.ls.fi.upm.es/svn/negation_and_fuzzy_logic/ciao-debian/
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
pushd ~/tmp

# Clean up
rm -fv $BUILD_TGZ $BUILD_DSC $BUILD_DIFF $BUILD_DIFF_GZ

# Ensure folder exists and has a pristine copy.
# rm -fR $FOLDER_NAME
mkdir -p $FOLDER_NAME
pushd $FOLDER_NAME

# Update and export the Ciao's repository
echo " "
if [ -d .svn ]; then
	echo "updating CIAO to revision $REVISION from $REPOS_1"
	svn revert -R .
	svn update --revision $REVISION
else
	echo "checking out CIAO to revision $REVISION from $REPOS_1"
	svn co $REPOS_1 . --revision $REVISION
fi
popd
echo " "

# Ensure folder exists.
# rm -fR $FOLDER_NAME/debian
mkdir -p $FOLDER_NAME/debian
pushd $FOLDER_NAME/debian

echo " "
if [ -d .svn ]; then
# if [ -d .git ]; then
    if [ "$DEBIAN_REPOS_REVISION" == "latest" ]; then
	echo "updating debian subfolder to last revision from $REPOS_2"
	svn revert -R .
	svn update
    else
	echo "updating debian subfolder to revision $DEBIAN_REPOS_REVISION from $REPOS_2"
	svn revert -R .
	svn update --revision $DEBIAN_REPOS_REVISION
	# git svn rebase
    fi
else
    if [ "$DEBIAN_REPOS_REVISION" == "latest" ]; then
	echo "checking out debian subfolder to last revision from $REPOS_2"
	svn co $REPOS_2 . 
	# git svn clone $REPOS_2 .
    else
	echo "checking out debian subfolder to revision $DEBIAN_REPOS_REVISION from $REPOS_2"
	svn co $REPOS_2 . --revision $DEBIAN_REPOS_REVISION
    fi
fi
popd
echo " "

pushd $FOLDER_NAME
# Apply patches to the ciao distribution.
echo " -> Applying patches in debian/patches to ciao distribution ..."
for file in debian/patches/* 
do
	if [ ! -d $file ] && [ ! "$file" == "." ] && [ ! "$file" == ".." ]; then 
		echo "Found file patch in $file"
	#	patch -p0 --forward --verbose < $file
	fi
done
echo " "
popd

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
echo " sudo cowbuilder --build $BUILD_DSC "
echo " "

popd
echo " "
echo " "

