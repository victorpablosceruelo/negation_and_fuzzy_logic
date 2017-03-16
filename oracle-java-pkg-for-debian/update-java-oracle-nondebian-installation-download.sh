#! /bin/bash

if [ -z "$1" ] || [ "$1" == "" ]; then
	echo " "
	echo "This script downloads java-oracle in your system."
	echo " "
	echo "usage: $0 url "
	echo "examples: "
	echo "   $0 http://download.oracle.com/otn-pub/java/jdk/7/jdk-7-linux-x64.tar.gz"
	echo "   $0 http://download.oracle.com/otn-pub/java/jdk/7u40-b43/jdk-7u40-linux-x64.tar.gz?AuthParam=1379179838_4cd3d6968ff470f88c76809a839a9f18"
	echo " "
	echo "you can download oracle java from: "
	echo " http://www.oracle.com/technetwork/java/javase/downloads/ "
	echo " "
	exit 0
fi

# wget --no-cookies --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com" "http://download.oracle.com/otn-pub/java/jdk/7/jdk-7-linux-x64.tar.gz"

wget --no-cookies --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com" "${1}"

#EOF

