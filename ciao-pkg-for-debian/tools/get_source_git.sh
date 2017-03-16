#!/bin/bash
set -x

# Create the package's name based on today's date.
#DATE=`date +%F+%H.%M`
DATE=`date +%Y%m%d`
PKGNAME=ciao-prolog-1.13+svn$DATE
TARNAME=ciao-prolog_1.13+svn$DATE.orig.tar.gz

# User
#SVNREPO=~/fuentes/ciao/ciao-svn
GITREPO=http://babel.ls.fi.upm.es/~egallego/repos/newciao.git
DEBIANREPO=http://babel.ls.fi.upm.es/~egallego/repos/ciao-debian.git

# Clone the Ciao's repository
git clone $GITREPO $PKGNAME
rm -rf $PKGNAME/.git

# Remove NODISTRIBUTE files from the original tarball.
# This is broken ATM
# pushd $PKGNAME
# CIAOSVNVER=`svnversion $SVNREPO`
# echo $CIAOSVNVER-debian > ./REVISION
# ./ciaosetup clean_nodistribute | sh
# popd

# Create .orig tarball

tar cvfz $TARNAME --exclude=.git $PKGNAME
pushd $PKGNAME
git clone $DEBIANREPO debian
rm -rf debian/.git
popd

# Create a suitable changelog entry.
pushd $PKGNAME
dch -d "New upstream snapshot"
popd
dpkg-source -b $PKGNAME
