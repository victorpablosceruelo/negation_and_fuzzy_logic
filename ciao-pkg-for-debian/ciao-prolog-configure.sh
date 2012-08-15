#!/bin/bash

if [ -z $1 ] || [ "$1" == "" ]; then
	echo "usage: $0 path_of_ciaosetup"
	exit 0
fi

#    --registration_type=all \
#    --instype=global \
#    --prefix=/usr \
#    --installgroup=root \
#    --update_bashrc=no \
#    --optim_level=normal \
#    --prefix=~/secured/local \
#    --htmldir=~/public_html/ciao-manual \
#    --htmlurl=http://localhost/ciao-manual \
#    --docdir=~/secured/local/share/doc/ciao-prolog \
#    --mandir=~/secured/local/share/man \
#    --infodir=~/secured/local/share/info \
#    --web_images_path=~/secured/local/share/doc/ciao-prolog/html \
#    --with_ciaoppcl=no \


./ciaosetup configure \
	--stop-if-error=yes \
	--registration_type=all \
	--instype=local \
	--prefix=/usr \
	--optimizing_compiler=no \
	--update_bashrc=no \
	--update_cshrc=no \
	--install_emacs_support=yes \
	--use_threads=yes \
	--optim_level=normal \
	--update_dotemacs=no \
	--docdir=/usr/share/doc/ciao-prolog \
	--mandir=/usr/share/man \
	--infodir=/usr/share/info \
	--web_images_path=/usr/share/doc/ciao-prolog/html \
	--htmldir=/usr/share/doc/ciao-prolog/html \
	--install_prolog_name=no --execmode=755 --datamode=644 \
	--installgroup=root \
	--with_gsl=no --with_ppl=no \
	--with_ant=yes --with_java_interface=yes \
	--with_mysql=yes --mysql_client_directory=/usr/lib

	# New in version 1.15.0 r14285
#	./$(CIAOSETUP) configure 
#	./$(CIAOSETUP) configure --setup
#	./$(CIAOSETUP) build
#	./$(CIAOSETUP) docs
#	./$(CIAOSETUP) install
#	./$(CIAOSETUP) install_to_destroot --destdir=[value]

# Add here commands to compile and install the package.
# ./$(CIAOSETUP) build
# ./$(CIAOSETUP) docs
# ./$(CIAOSETUP) install_to_destroot --destdir=$(CURDIR)/debian/tmp
# ./$(CIAOSETUP) install_to_destroot --destdir=/usr

# EOF
