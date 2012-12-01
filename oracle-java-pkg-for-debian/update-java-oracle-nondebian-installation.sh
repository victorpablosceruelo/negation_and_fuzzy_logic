#! /bin/bash

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ]; then
	echo " "
	echo "This script installs java-oracle in your system from a tar.gz file you must download from oracle web page."
	echo " "
	echo "usage: sudo $0 oracle-java-jdk.tar.gz destiny_folder"
	echo "example: sudo $0 Desktop/jdk-7u9-linux-x64.tar.gz /opt/oracle-java"
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

JAVA_INSTALLATION_DIR="${2}"
SUBDIR_NAME="`basename ${1} .tar.gz | sed 's/\.tar\.gz$//'`"
FILENAME="${SUBDIR_NAME}.tar.gz"
LOGFILE=".java-oracle-update.txt"
mkdir -p ${JAVA_INSTALLATION_DIR}

if [ ! -d ${JAVA_INSTALLATION_DIR} ]; then
	echo "ERROR: destiny folder does not exist. "
	exit 0
fi

pushd ${JAVA_INSTALLATION_DIR}
echo "Removing subdirectory ${SUBDIR_NAME} in ${JAVA_INSTALLATION_DIR} "
if [ -f ${LOGFILE} ]; then 
    rm -fv ${LOGFILE}
fi
touch ${LOGFILE}
if [ ! -f ${LOGFILE} ]; then 
    echo "Cannot create ${LOGFILE} file."
    echo "Maybe you have no permissions to write?"
    exit 0
fi
rm -fvR ${SUBDIR_NAME} > ${LOGFILE}
mkdir -pv ${SUBDIR_NAME}
popd

echo "Copying file ${1} to ${JAVA_INSTALLATION_DIR}/${SUBDIR_NAME} "
cp -vi ${1} ${JAVA_INSTALLATION_DIR}/${SUBDIR_NAME} 
pushd ${JAVA_INSTALLATION_DIR}
pushd ${SUBDIR_NAME}

echo "Uncompressing file ${FILENAME} to ${JAVA_INSTALLATION_DIR}/${SUBDIR_NAME} "
rm -fv ${LOGFILE}
touch ${LOGFILE}
chmod 777 ${LOGFILE}
tar zxvf ${FILENAME} >> ${LOGFILE}
chmod 444 ${LOGFILE}

echo "Removing file ${FILENAME}"
rm -fv ${FILENAME}

echo "Setting the subfolder name to use ... "
for file in * ; do
	if [ ! -z $file ] && [ ! "$file" == "" ] && [ ! "$file" == "." ] && [ ! "$file" == ".." ] && [ ! "$file" == "*" ] && [ ! "$file" == "${LOGFILE}" ]; then
		DIRNAME="$file"
	fi
done
popd
echo "Subfolder name: $DIRNAME"

if [ -z "${DIRNAME}" ] || [ "${DIRNAME}" == "" ]; then
	echo "ERROR: DIRNAME is not set. "
	exit 0
fi

echo "Testing that everything is as expected ... "
if [ -z "${SUBDIR_NAME}" ] || [ "${SUBDIR_NAME}" == "" ] || [ "${SUBDIR_NAME}" == "*" ]; then
    echo "The variable SUBDIR_NAME contains an invalid value. SUBDIR_NAME: ${SUBDIR_NAME}"
    exit 0
fi
if [ -z "${DIRNAME}" ] || [ "${DIRNAME}" == "" ] || [ "${DIRNAME}" == "*" ]; then
    echo "The variable DIRNAME contains an invalid value. DIRNAME: ${DIRNAME}"
    exit 0
fi
if [ "${DIRNAME}" == "${SUBDIR_NAME}" ]; then
    echo "The variables DIRNAME and SUBDIR_NAME contain the same value. DIRNAME: ${DIRNAME}"
    exit 0
fi

echo "Removing old version in ${DIRNAME}"
rm -fRv ${DIRNAME} >> ${LOGFILE}
echo "Moving new version to ${DIRNAME} "
mv -v ${SUBDIR_NAME}/${DIRNAME} .

popd 

echo " "
echo "Updating alternatives using the script" 
echo "/usr/share/oracle-java-pkg-for-debian/update-java-oracle-nondebian-installation-aux.sh ... "
echo " "
/usr/share/oracle-java-pkg-for-debian/update-java-oracle-nondebian-installation-aux.sh ${JAVA_INSTALLATION_DIR} ${DIRNAME}
echo "END."

#EOF

