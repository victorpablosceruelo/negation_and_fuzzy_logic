#!/bin/bash

echo "running $0 $* ... "

if [ -z $1 ] || [ "$1" == "" ] || [ -z $2 ] || [ "$2" == "" ] || [ -z $3 ] || [ "$3" == "" ]; then
	echo " "
	echo "ERROR: This arguments needs three arguments to be run."
	echo "${0} CURRENT_DIR PKG_FINAL_PATH PKG_FINAL_SUBPATH"
	echo " "
	exit -1
fi

CURRENT_DIR="${1}" 
PKG_FINAL_PATH="${2}" 
PKG_FINAL_SUBPATH="${3}"
FULL_PATH="${PKG_FINAL_PATH}/${PKG_FINAL_SUBPATH}/"
LOGS_PATH="${FULL_PATH}/debian-pkg-logs"
DATE=`date '+%Y%m%d_%H%M%S'`
LOGS_FILE="${LOGS_PATH}/folders-status-check_${DATE}.txt"

# ensure folders exist.
mkdir -p ${PKG_FINAL_SUBPATH}
mkdir -p ${PKG_FINAL_PATH}
mkdir -p ${FULL_PATH}
mkdir -p ${LOGS_PATH}

echo "Listing the files in ${CURRENT_DIR}, ${PKG_FINAL_PATH} and ${FULL_PATH} "
echo " " >> ${LOGS_FILE}
echo "Files in ${CURRENT_DIR}:" >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
find ${CURRENT_DIR} 2>&1 >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
echo "Files in ${PKG_FINAL_PATH}:" >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
find ${PKG_FINAL_PATH} 2>&1 >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
echo "Files in ${FULL_PATH}:" >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
find ${FULL_PATH} 2>&1 >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
echo " " >> ${LOGS_FILE}
echo "Done."
echo " "

# Compress to save space, and remove just after.
tar -cjvf ${LOGS_FILE}.tar.bz2 ${LOGS_FILE}
rm -fv ${LOGS_FILE}
ls -la ${LOGS_FILE}.tar.bz2