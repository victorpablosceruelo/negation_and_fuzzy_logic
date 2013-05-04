#!/bin/sh
##
## createdist.sh
## 
## Made by Edison Mera
## Modified by Jose F. Morales (merging all distribution scripts)
## 
## Started on  Fri Sep  7 17:12:22 2007 Edison Mera
##

set -e

# Put in 'self' the directory this script lives in
get_mydir() {
  old_dir=`pwd`
  cd `dirname $0`
  dir=`pwd`
  cd ${old_dir}
  echo ${dir}
}
self=`get_mydir`
##

# ---------------------------------------------------------------------------
# Messages

message() {
    echo "{CiaoWebsite: $*}"
}

bug() {
    echo "{CiaoWebsite: Bug: $*}"
    false
}

error() {
    echo "{CiaoWebsite: Error: $*}"
    false
}

# ---------------------------------------------------------------------------
# Some configuration paths

# Setup variables for a given distribution type
setup_dist() { # Disttype
    DISTTYPE=${1}

    # Values of CLIPDIST:
    #   yes - install in clip machine
    #   no  - do a personal instalation
    case ${DISTTYPE} in
	New) CLIPDIST=yes; REMOVE_OLD_PACKAGES=yes; FETCH_NEW_PACKAGES=yes ;;
	testing)  CLIPDIST=no; REMOVE_OLD_PACKAGES=no; FETCH_NEW_PACKAGES=no ;;
#	personal) CLIPDIST=no; REMOVE_OLD_PACKAGES=no; FETCH_NEW_PACKAGES=yes ;;
	*) bug "Unknown value '${DISTTYPE}' for DISTTYPE"
    esac

    TESTING_ACCOUNT=ciaotester@cliptest1.dia.fi.upm.es

    CIAODIR=${self}/..
    if [ x${CLIPDIST} = x"yes" ] ; then
	message "Preparing distribution web page in clip machine"
	message "(assuming a working local installation)"
        # Settings for a installation in the clip machine (using latest working sources)
        # The repository of Ciao (unless it is already set)
	# CIAOREPO=file:///home/clip/SvnReps/Systems/CiaoDE/trunk/
        # Temporal place for the distribution
	# CIAODIR=/home/clip/tmp/distweb-${DISTTYPE}/CiaoDE
        # Where the web pages will go
	DISTHTMLDIR="/home/clip/public_html/Software/Ciao"
	DISTHTMLURL=""
#	DISTHTMLURL="${DISTTYPE}/"
#	DISTHTMLURL="/Software/Ciao/${DISTTYPE}/"
# 	DISTHTMLURL="http://clip.dia.fi.upm.es/Software/Ciao/${DISTTYPE}/"
    else
	message "Preparing a local distribution web page"
	message "(assuming a working local installation)"
	# Settings for a personal installation from current sources
	DISTHTMLDIR="${HOME}/public_html/CiaoDE"
	if [ x${DISTTYPE} = xpersonal ] ; then
	    DISTHTMLURL="/~${USER}/CiaoDE/"
	else
	    DISTHTMLURL="file://${HOME}/public_html/CiaoDE/"
	fi
    fi
    LPDOC=${CIAODIR}/build/bin/lpdoc
}

# ---------------------------------------------------------------------------

# Update the pbundle local storage (only for 'trunk')

# TODO: packages for branches (including stable versions) are not
#       removed or fetched! I dare doing that automatically, I prefer
#       doing if by hand at this moment... (JFMC)

update_local_pbundle_storage() {
    CIAOC=${CIAODIR}/build/bin/ciaoc
    cd ${CIAODIR}/website
    ${CIAOC} ${CIAODIR}/website/lpdist.pl
    LPDIST=${CIAODIR}/website/lpdist
    #
    if [ x"${REMOVE_OLD_PACKAGES}" = x"yes" ] ; then
	DIRS_TO_REMOVE=`${LPDIST} wipe-list trunk`
	if [ x"${DIRS_TO_REMOVE}" = x"" ]; then
	    message "No outdated pbundle found"
	else
	    message "Deleting ${DIRS_TO_REMOVE}"
	    rm -rf ${DIRS_TO_REMOVE}
	fi
    fi
    #
    if [ x${FETCH_NEW_PACKAGES} = x"yes" ] ; then
	${LPDIST} fetch-latest
    fi
}

# ---------------------------------------------------------------------------
# Create the distribution

# A replace string filter that works for any combination of ':' and
# '/' characters.
# todo: the whole program could be simplified so that this is not necessary
replace_string() { # From, To
    sed -e 's/'$(echo ${1} | sed -e 's:/:\\/:g')'/'$(echo ${2} | sed -e 's:/:\\/:g')'/g'
}

patch_lpsettings() {
    # Patch SETTINGS_skel.pl
    DISTOWNER=`id -nu`
    DISTGROUP=`id -ng`
    cat ${CIAODIR}/website/SETTINGS_skel.pl | \
    replace_string "<v>DistOwner</v>" "${DISTOWNER}" | \
      replace_string "<v>DistGroup</v>" "${DISTGROUP}" | \
      replace_string "<v>DistHtmlDir</v>" "${DISTHTMLDIR}/" | \
      replace_string "<v>DistHtmlURL</v>" "${DISTHTMLURL}" | \
      replace_string "<v>WebsiteDir</v>" "${self}" \
      > ${CIAODIR}/website/SETTINGS.pl
}

# ---------------------------------------------------------------------------

update_website() {
    # Update the website
    mkdir -p ${DISTHTMLDIR}
    OLDPWD=`pwd`
    cd ${DISTHTMLDIR}
    # Do not do a realclean or you will lose any stored documentation 
    # ${LPDOC} -f ${CIAODIR}/website/SETTINGS realclean # TEMPORAL?
    ${LPDOC} -f ${CIAODIR}/website/SETTINGS clean # TEMPORAL?
    ${LPDOC} -f ${CIAODIR}/website/SETTINGS all
    cd ${OLDPWD}
}

# ---------------------------------------------------------------------------

help() {
    cat <<EOF
Usage: `basename ${0}` [personal|testing|New]
EOF
    exit 1
}

# Check 
case ${1} in
#    personal) true ;;
    testing) true ;;
    New) true ;;
    *)    help ;;
esac

cd ${self}
setup_dist ${1}
patch_lpsettings

update_local_pbundle_storage

update_website





