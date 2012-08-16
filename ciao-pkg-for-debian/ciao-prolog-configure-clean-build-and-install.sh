#!/bin/bash

echo "running $0 $* ... "
read -t 60 -p "Press enter to continue with Ciao Prolog INSTALLATION"

CIAOSETUP=ciaosetup

echo " "
echo " "
./${CIAOSETUP} clean
echo " "
echo " "
./${CIAOSETUP} clean_config
echo " "
echo " "
./${CIAOSETUP} realclean
echo " "
echo " "
./${CIAOSETUP} braveclean
echo " "
echo " "

# Facility to use always the same configuration
./ciao-prolog-configure.sh ${CIAOSETUP}
echo " "
echo " "

# Add here commands to compile the package.
./${CIAOSETUP} build
echo " "
echo " "
./${CIAOSETUP} docs
echo " "
echo " "

read -t 60 -p "Press enter to continue with Ciao Prolog INSTALLATION"
echo " "
echo " "

# And the commands to install it.
./${CIAOSETUP} install
echo " "
echo " "

# EOF
