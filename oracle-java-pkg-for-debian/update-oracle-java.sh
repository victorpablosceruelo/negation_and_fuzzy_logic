#! /bin/sh

if [ -z "$1" ] || [ "$1" == "" ]; then
	echo "usage: $0 oracle-java-jdk.tar.gz "
	echo "example: $0 jdk-7u9-linux-x64"
	echo "you need super-user rights to run this script."
	exit 0
fi

if [ ! -f ${1}.tar.gz ]; then
	echo "file ${1}.tar.gz does not exist. Aborting."
fi

FILENAME="${1}.tar.gz"
DIRNAME="${1}"
JAVA_INSTALLATION_DIR="/opt"
JAVA_LINK="java_jre_oracle"

sudo mkdir -p ${JAVA_INSTALLATION_DIR}
sudo mv ${FILENAME} ${JAVA_INSTALLATION_DIR}

pushd ${JAVA_INSTALLATION_DIR}

tar zxvf ${FILENAME}
rm -fv ${JAVA_LINK}
ln -s ${DIRNAME} ${JAVA_LINK}

popd

/usr/share/oracle-java-pkg-for-debian ${JAVA_INSTALLATION_DIR}/${JAVA_LINK} 

#EOF

