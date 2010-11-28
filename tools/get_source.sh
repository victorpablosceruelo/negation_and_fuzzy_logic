#!/bin/bash
if [ -z $1 ]
then
    echo "Usage: $0 ciao_repository_path"
    exit
fi

set -x

# Create the package's name based on today's date.
#DATE=`date +%F+%H.%M`
DATE=`date +%Y%m%d`
PKGNAME=ciao-prolog-1.13+svn$DATE
TARNAME=ciao-prolog_1.13+svn$DATE.orig.tar.gz

# User
#SVNREPO=~/fuentes/ciao/ciao-svn
SVNREPO=$1

# Update and export the Ciao's repository
pushd $SVNREPO
svn up
popd
svn export $SVNREPO ./$PKGNAME

# Remove NODISTRIBUTE files from the original tarball.
# This is broken ATM
# pushd $PKGNAME
# CIAOSVNVER=`svnversion $SVNREPO`
# echo $CIAOSVNVER-debian > ./REVISION
# ./ciaosetup clean_nodistribute | sh
# popd

# Create .orig tarball
tar cvfz $TARNAME $PKGNAME
svn export .. ./$PKGNAME/debian

# Create a suitable changelog entry.
pushd $PKGNAME
dch -d "New upstream snapshot"
popd
dpkg-source -b $PKGNAME
