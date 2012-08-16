#!/bin/bash

echo "running $0 $* ... "

CIAOSETUP=ciaosetup

@echo " "
./$(CIAOSETUP) clean
@echo " "
./$(CIAOSETUP) clean_config
@echo " "
./$(CIAOSETUP) realclean
@echo " "
./$(CIAOSETUP) braveclean
@echo " "

# Facility to use always the same configuration
./debian/ciao-prolog-configure.sh $(CIAOSETUP)

# Add here commands to compile the package.
./$(CIAOSETUP) build
./$(CIAOSETUP) docs

# And the commands to install it.
./$(CIAOSETUP) install

