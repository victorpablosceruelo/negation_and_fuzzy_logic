#!/bin/bash

# Automatizando tareas "rollo"

DIR=`dirname $0`
mkdir -pv /var/cache/local-debs/
mv -v /var/cache/pbuilder/result/* /var/cache/local-debs/
$DIR/rebuild-repo-packages-list.sh /var/cache/local-debs/
echo " "
echo " "
rm -fv /etc/apt/sources.list.d/local-debs.list
echo "### THIS FILE IS AUTOMATICALLY CONFIGURED ###" >> /etc/apt/sources.list.d/local-debs.list
echo "# Local debs" >> /etc/apt/sources.list.d/local-debs.list
echo "deb file:/var/cache/local-debs/ ./" >> /etc/apt/sources.list.d/local-debs.list
echo " "
echo " "
apt-get update
# aptitude
apt-get install ciao-prolog
