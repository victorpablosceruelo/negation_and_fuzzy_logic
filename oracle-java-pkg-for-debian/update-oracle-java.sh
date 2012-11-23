#! /bin/sh

if [ -z "$1" ] || [ "$1" == "" ]; then
	echo " "
	echo "usage: $0 oracle-java-jdk.tar.gz "
	echo "example: $0 jdk-7u9-linux-x64"
	echo "you need super-user rights to run this script."
	echo " "
	echo "you can download oracle java from: "
	echo " http://www.oracle.com/technetwork/java/javase/downloads/ "
	echo " "
	exit 0
fi

if [ ! -f ${1} ]; then
	echo " "
	echo "file ${1} does not exist. Aborting."
	echo " "
	exit 0
fi

TMP_FILENAME="`basename ${1} .tar.gz`"
TMP_FILENAME=`echo "${TMP_FILENAME}"|sed 's/\.tar\.gz$//'`

FILENAME="${TMP_FILENAME}.tar.gz"
DIRNAME="${1}"
JAVA_INSTALLATION_DIR="/opt"
JAVA_LINK="java_jre_oracle"

sudo mkdir -p ${JAVA_INSTALLATION_DIR}
sudo cp ${1} ${JAVA_INSTALLATION_DIR}

if [ ! -f ${JAVA_INSTALLATION_DIR}/${FILENAME} ]; then
	echo "ERROR: Filename does not exist: ${JAVA_INSTALLATION_DIR}/${FILENAME}"
	exit 0
fi
if [ ! -d ${JAVA_INSTALLATION_DIR} ]; then
	echo "ERROR: destiny folder does not exist. "
	exit 0
fi

pushd ${JAVA_INSTALLATION_DIR}

sudo rm -fR ${TMP_FILENAME}
sudo touch .oracle-java-update.txt
sudo chmod 777 .oracle-java-update.txt
sudo tar zxvf ${FILENAME} >> .oracle-java-update.txt
sudo chmod 444 .oracle-java-update.txt
sudo rm -fv ${JAVA_LINK}
sudo ln -s ${DIRNAME} ${JAVA_LINK}

popd

sudo /usr/share/oracle-java-pkg-for-debian/oracle-java-nondebian-installation.sh ${JAVA_INSTALLATION_DIR}/${JAVA_LINK} 

#EOF

