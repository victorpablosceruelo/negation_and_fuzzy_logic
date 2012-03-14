#!/bin/bash

PACKAGES="build-essential debhelper texinfo texi2html texlive-latex-base tetex-bin texlive-latex-extra tetex-extra dvi2ps dvi2dvi gs-gpl gs gs-common ghostscript subversion locate emacs imagemagick gcc-multilib libc6-dev-i386 ia32-libs g++-multilib libgsl0-dev virtual-mysql-client mysql-server mysql-common libmysql-java libmysql++-dev libmysqlclient16 libmysqlclient-dev libmysqld-dev libmysql-cil-dev libppl-c-dev ant sun-java6-demo sun-java6-javadb sun-java6-source sun-java6-jdk bash bash-static"

echo " "
INSTALLED=""
UNINSTALLED=""
for pkg in $PACKAGES
do
    TEST=`apt-cache policy $pkg  | grep "Installed: (none)"`
    INFO=`apt-cache policy $pkg  | grep "Installed:"`
    if [ -z "$TEST" ] || [ "$TEST" == "" ]; then
#	MSG=`echo $INFO | grep "Installed:"`
	INSTALLED="${INSTALLED}  ${pkg}: ${INFO}, "
    else
	echo "${pkg}: $INFO "
	UNINSTALLED="$UNINSTALLED $pkg"
    fi
done
echo " "
echo "INSTALLED: $INSTALLED"
echo " "
echo "UNINSTALLED: $UNINSTALLED"
echo " "
echo "aptitude install $UNINSTALLED"
echo " "