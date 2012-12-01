#! /bin/bash

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

FILENAME_AUX="`basename ${1} .tar.gz`"
FILENAME_AUX=`echo "${FILENAME_AUX}"|sed 's/\.tar\.gz$//'`

FILENAME="${FILENAME_AUX}.tar.gz"
JAVA_INSTALLATION_DIR="/opt"
JAVA_LINK_1="java_jre_oracle"
JAVA_LINK_2="java_oracle"

sudo mkdir -p ${JAVA_INSTALLATION_DIR}

if [ ! -d ${JAVA_INSTALLATION_DIR} ]; then
	echo "ERROR: destiny folder does not exist. "
	exit 0
fi

pushd ${JAVA_INSTALLATION_DIR}
echo "Removing ${FILENAME_AUX} ... "
sudo rm -fR ${FILENAME_AUX}
sudo mkdir -p ${FILENAME_AUX}
popd

sudo cp -vi ${1} ${JAVA_INSTALLATION_DIR}/${FILENAME_AUX} 
pushd ${JAVA_INSTALLATION_DIR}
pushd ${FILENAME_AUX}

sudo touch .oracle-java-update.txt
sudo chmod 777 .oracle-java-update.txt
sudo tar zxvf ${FILENAME} >> .oracle-java-update.txt
sudo chmod 444 .oracle-java-update.txt
sudo rm -fv ${FILENAME}

for file in * ; do
	if [ ! -z $file ] && [ ! "$file" == "" ] && [ ! "$file" == "." ] && [ ! "$file" == ".." ] && [ ! "$file" == "*" ]; then
		DIRNAME="$file"
	fi
done
popd

if [ -z "${DIRNAME}" ] || [ "${DIRNAME}" == "" ]; then
	echo "ERROR: DIRNAME is not set. "
	exit 0
fi

if [ ! -z "${FILENAME_AUX}" ] && [ ! -z "${DIRNAME}" ] && [ ! "${FILENAME_AUX}" == "" ] && [ ! "${DIRNAME}" == "" ] && [ ! "${FILENAME_AUX}" == "*" ] && [ ! "${DIRNAME}" == "*" ]; then
	echo "Removing old version in ${DIRNAME}"
	sudo rm -fR ${DIRNAME} 
	echo "Moving new version to ${DIRNAME} "
	sudo mv -v ${FILENAME_AUX}/${DIRNAME} .
fi

sudo rm -fv ${JAVA_LINK_1} ${JAVA_LINK_2}
echo "Building new links for ${JAVA_LINK_1} and ${JAVA_LINK_2} ... "
sudo ln -vs ${DIRNAME} ${JAVA_LINK_1}
sudo ln -vs ${DIRNAME} ${JAVA_LINK_2}

popd 

echo "Updating alternatives ... "
sudo /usr/share/oracle-java-pkg-for-debian/oracle-java-nondebian-installation.sh ${JAVA_INSTALLATION_DIR}/${JAVA_LINK_2} 
echo "END."

#EOF

