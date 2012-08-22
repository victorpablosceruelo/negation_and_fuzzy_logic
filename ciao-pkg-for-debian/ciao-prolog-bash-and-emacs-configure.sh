#!/bin/bash

if [ -z $1 ] || [ "$1" == "" ]; then
    echo "This script modifies .emacs and .bashrc to add support for Ciao Prolog."
    echo "usage: $0 home_path"
    exit -1
fi

HOME_PATH="$1"

function test_retval() {
    if [ -z $1 ] || [ "$1" == "" ] || [ ! "$1" == "0" ]; then
	echo " "
	echo "Return value is: --${1}-- "
	echo " "
	exit $1
    fi
}


if [ ! -d ${HOME_PATH} ]; then
    mkdir -pv ${HOME_PATH} 
    test_retval $?
fi

test="`cat ${HOME_PATH}/.bashrc | grep \"Ciao Prolog Support\"`"
if [ -z "$test" ] || [ "$test" == "" ]; then
    echo " " >> ${HOME_PATH}/.bashrc
    echo "# Ciao Prolog Support " >> ${HOME_PATH}/.bashrc
    echo "if [ -f /usr/share/CiaoDE/ciao/etc/DOTprofile ] ; then" >> ${HOME_PATH}/.bashrc
    echo "    . /usr/share/CiaoDE/ciao/etc/DOTprofile" >> ${HOME_PATH}/.bashrc
    echo "fi " >> ${HOME_PATH}/.bashrc
    echo " " >> ${HOME_PATH}/.bashrc
else
    echo "Remove previously installed support for Ciao Prolog in .bashrc."
fi

test="`cat ${HOME_PATH}/.emacs | grep \"Ciao Prolog Support\"`"
if [ -z "$test" ] || [ "$test" == "" ]; then
    echo " " >> ${HOME_PATH}/.emacs
    echo ";; Ciao Prolog Support " >> ${HOME_PATH}/.emacs
    echo "(if (file-exists-p \"/usr/share/CiaoDE/emacs-mode/ciao-mode-init.el\")" >> ${HOME_PATH}/.emacs
    echo "     (load-file \"/usr/share/CiaoDE/emacs-mode/ciao-mode-init.el\")" >> ${HOME_PATH}/.emacs
    echo ")" >> ${HOME_PATH}/.emacs
    echo " " >> ${HOME_PATH}/.emacs
else 
    echo "Remove previously installed support for Ciao Prolog in .emacs."
fi