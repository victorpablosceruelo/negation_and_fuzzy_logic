#!/bin/bash

if [ -z "$1" ] || [ "$1" == "" ]; then
	echo "First arg is where is the repos."
	exit 0
fi

if [ -d "$1" ]; then
	DIR="$1"
else
	echo "$1 is not a directory."
	exit 0
fi

# DIR=~/public_html/others/packages/debian

pushd $DIR

echo "Removing old files ..."
rm -fv Packages Sources Release Packages.bz2 Sources.bz2 Release.bz2

echo "Packages"
apt-ftparchive packages . > Packages 
echo "Sources"
apt-ftparchive sources  . > Sources
echo "Release"
apt-ftparchive release  . > Release
echo "Removing old files ..."
rm -fv *.bz2
echo "Compressing ... "
bzip2 -v Packages && rm -fv Packages
bzip2 -v Sources  && rm -fv Sources
bzip2 -v Release  && rm -fv Release

popd
