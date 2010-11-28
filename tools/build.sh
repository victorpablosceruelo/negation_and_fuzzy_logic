#!/bin/bash
set -x
#DATE=`date +%F+%H.%M`
DATE=`date +%Y%m%d`
PKGNAME=ciao-prolog-1.13+svn$DATE
TARNAME=ciao-prolog_1.13+svn$DATE.orig.tar.gz
SVNREPO=~/fuentes/ciao/ciao-svn
CIAOSVNVER=`svnversion $SVNREPO`

pushd $SVNREPO
svn up
popd
svn export $SVNREPO ./$PKGNAME
# This is broken ATM
pushd $PKGNAME
#echo $CIAOSVNVER-debian > ./REVISION
# This is broken ATM
# ./ciaosetup clean_nodistribute | sh
popd
tar cvfz $TARNAME $PKGNAME
svn export .. ./$PKGNAME/debian
dpkg-source -b $PKGNAME
