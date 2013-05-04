#!/bin/sh
##
## scpcmd.sh
## 
## Made by Edison Mera
## Login   <edison@clip.dia.fi.upm.es>
## 
## Started on  Thu Jul 23 15:04:20 2009 Edison Mera
## Last update Mon Jul 27 18:37:34 2009 Edison Mera
##

SOURCE="$1"
TARSRC="$2"
SSHCMD="$3"
TARGET="$4"
TARTGT="$5"

if [ -d "${SOURCE}" ] ; then
    ( cd "${SOURCE}" ; ${TARSRC} -c * | ${SSHCMD} "mkdir -p ${TARGET} ; ${TARTGT} -C ${TARGET} -x" )
elif [ -f "${SOURCE}" ] ; then
    cat "${SOURCE}" | ${SSHCMD} "cat > ${TARGET}"
else
    echo "ERROR: Unable to read ${SOURCE} file"
    exit 1
fi
