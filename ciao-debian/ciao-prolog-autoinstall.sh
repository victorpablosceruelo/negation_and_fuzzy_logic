#!/bin/bash

# Automatizando tareas "rollo"

mkdir -p /var/cache/local-debs/
mv /var/cache/pbuilder/result/* /var/cache/local-debs/

rm -fv /etc/apt/sources.list.d/local-debs
echo "### THIS FILE IS AUTOMATICALLY CONFIGURED ###" >> /etc/apt/sources.list.d/local-debs
echo "# Local debs" >> /etc/apt/sources.list.d/local-debs
echo "deb file:/var/cache/local-debs/ ./" >> /etc/apt/sources.list.d/local-debs

apt-get update
# aptitude
apt-get install ciao-prolog