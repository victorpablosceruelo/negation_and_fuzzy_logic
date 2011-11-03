#!/bin/bash
# FILE: /etc/apt/git-snapshot-script.sh

INSTALLED_PACKAGES_LIST_FILE="apt/installed_packages_list.txt"
INSTALLED_PACKAGES_LIST_WITH_VERSIONS_FILE="apt/installed_packages_list_with_versions.txt"
TBZ_FILE="apt-configuration.tbz"
LOGS_FILE="apt-logs.txt"
EMAILS="victorpablos@yahoo.com victorpablosceruelo.logs@gmail.com"
MAILER="/usr/bin/mail"

IMPORTANT_FOLDERS_AND_FILES="/etc/apt /etc/scripts /etc/masqmail"

# This ones should not be touched.
# CALLER=$(ps axww | grep "^ *$$" -B3 | grep " apt-get " | head -n1 | sed 's/^.*\(apt-get .*\)/\1/' )
# CALLER=$(ps axww | mawk '/aptitude|apt-get/ {for (i=5; i<=NF ; i++) printf ("%s ",$i); printf ("\n") }' | head -1)
CALLER=$(ps axww | grep "aptitude\|apt-get" | grep -v grep | head -1)
LINE_ORDER=$(ps axww | grep "aptitude\|apt-get" | grep -v grep)

set -e
echo "$0: ---------------------------------------------------------"
echo -n "$0: enter directory "
pushd /etc/

if [ -z "$CALLER" ] || [ "$CALLER" == "" ]; then
	CALLER="unknownApp"
else
	CALLER="aptitude/apt-get"
fi

if [ -z "$1" ] || [ "$1" == "" ]; then
	OPERATION="unknownOp"
else
	case "$1" in
		pre) OPERATION="before"
			;;
		post) OPERATION="after"
			;;
		commit) OPERATION="at `date` by"
			;;
		*) OPERATION="unknownOp"
			;;
	esac
fi

# Logging message ...
echo "$0: command line: $0 $1 $2"
echo "$0: Testing repos status $OPERATION: $CALLER ..."

echo "$0: Cleaning previous logs. "
rm -f "$LOGS_FILE"
touch "$LOGS_FILE"

echo "$0: Updating system selected packages' list. "
dpkg --get-selections > "$INSTALLED_PACKAGES_LIST_FILE"
dpkg -l > "$INSTALLED_PACKAGES_LIST_WITH_VERSIONS_FILE"

if [ ! -x /usr/bin/git ]; then
	echo " "
	echo "Install GIT to track changes in etc repository."
	echo " "
	exit 0
fi

if [ ! -f .gitignore ]; then 
	touch .gitignore
fi

function test_gitignore_file_for() {
    TEST="$(cat .gitignore | grep $1)" || true
    if [ -z "$TEST" ]; then
	echo "$0: Using .gitignore to ignore changes on $1 "
	echo "$1" >> .gitignore
    else
	echo "$0: Git repository ignores changes on $1 "
    fi
}

# File containing logs should not be in repository.
test_gitignore_file_for "$LOGS_FILE"
# File containing a compressed version of the apt config files
# should not be in repository.
test_gitignore_file_for "$TBZ_FILE"

function test_and_commit_changes_for() {

    case "$1" in
	etc)
	    MSG="etc dir"
	    FILES="/etc"
	    ;;
	apt)
	    MSG="apt dir"
	    FILES="/etc/apt"
	    ;;
	scripts)
	    MSG="scripts dir"
	    FILES="/etc/scripts"
	    mkdir -p $FILES
	    ;;
	pkgs_lists_files)
	    MSG="pkgs lists files"
	    FILES="$INSTALLED_PACKAGES_LIST_FILE $INSTALLED_PACKAGES_LIST_WITH_VERSIONS_FILE"
	    ;;
	file-owners-and-permissions)
		MSG="file owners and permissions"
		FILES=".git_cache_meta"
		;;
    esac

    # Test if we need to commit changes.
    TEST_1="$(git status $FILES | grep "\(Changed\|Changes\|Untracked\)" )" || true
    # TEST_1="$(git diff $INSTALLED_PACKAGES_LIST_FILE $INSTALLED_PACKAGES_LIST_WITH_VERSIONS_FILE)" || true

    if [ -z "$TEST_1" ] || [ "" == "$TEST_1" ]; then
	echo "$0: No changes to commit for $MSG."
    else
	echo " " >> "$LOGS_FILE"
	echo "$(git status $FILES)" >> "$LOGS_FILE"
	echo " " >> "$LOGS_FILE"
	TEST_1="$(git status $FILES | grep "\(Modified\|modified\)" )" || true
	if [ -z "$TEST_1" ] || [ "" == "$TEST_1" ]; then
		echo " " >> "$LOGS_FILE"
	else
		echo "$(git diff $FILES)" >> "$LOGS_FILE"
		echo " " >> "$LOGS_FILE"
	fi
	echo "$0: Commiting changes for $MSG."
	git add $FILES
	echo " "
	git commit -m "$0: snapshot for $MSG $OPERATION: $CALLER." $FILES
    fi;
}

test_and_commit_changes_for pkgs_lists_files
test_and_commit_changes_for apt
test_and_commit_changes_for scripts
test_and_commit_changes_for etc

# Keep file permissions/owners
# From http://stackoverflow.com/questions/3207728/retaining-file-permissions-with-git
# From http://permalink.gmane.org/gmane.comp.version-control.git/105133||PATCH
#
echo "$0: Tracking file permissions/owners with git-cache-meta.sh"
./scripts/git-cache-meta.sh --store
# echo " "

test_and_commit_changes_for file-owners-and-permissions

# Make a compressed copy of config files.
echo " "
rm -f "$TBZ_FILE"
touch "$TBZ_FILE"

if [ -x /bin/tar ] && [ -x /bin/bzip2 ]; then
	echo "$0: Compressing config files. "
	tar jcf "$TBZ_FILE" $IMPORTANT_FOLDERS_AND_FILES
else
	echo "$0: No /bin/tar or /bin/bzip2 to compress config files. Install them. "
fi;

# Test if we need to send an email with committed changes.
TEST="$(cat "$LOGS_FILE")" || true

if [ -z "$TEST" ]; then
	echo "$0: Not sending email because nothing has changed."
else
# 	if [`wc -l "$LOGS_FILE"`]; then
	# Build email.
	HOSTNAME=`hostname -f`
	EMAIL_BODY=`echo " "; echo "Changes for hostname: $HOSTNAME "; echo " "`
	EMAIL_BODY=`echo "$EMAIL_BODY" ; echo "When running: $LINE_ORDER"; echo " " `
	EMAIL_BODY=`echo "$EMAIL_BODY" ; echo " " ; echo "cat $LOGS_FILE" ; echo " " `
	EMAIL_BODY=`cat $LOGS_FILE; echo " " ; echo " " `

	# Attached files new to be compressed.
	if [ -x /usr/bin/uuencode ] && [ -f "$TBZ_FILE" ]; then
		EMAIL_BODY=`echo "$EMAIL_BODY" ; uuencode "$TBZ_FILE" "$TBZ_FILE"`
	else
		echo " "
		echo "$0: WARNING: Please install sharutils to have uuencode."
		EMAIL_BODY=`echo "WARNING: Please install sharutils to have uuencode."`
	fi

	# Send email.
	if [ -x $MAILER ]; then
		echo "$0: Sending email with changes to $EMAILS."
		echo "$EMAIL_BODY" | $MAILER -s "$HOSTNAME: config files commit $OPERATION: $CALLER" $EMAILS
	else
		echo " "
		echo "$0: WARNING: No available mailer to send email. Sorry."
	fi
fi

echo -n "$0: exit directory "
popd
echo "$0: ---------------------------------------------------------"
echo " "

GIT_BACKUPS="/git_backups"
ETC_GIT_BACKUP="$HOSTNAME.etc.git"
echo "$0: performing ETC BACKUP in $GIT_BACKUPS/$ETC_GIT_BACKUP:"
mkdir -p $GIT_BACKUPS

if [ -d $GIT_BACKUPS/$ETC_GIT_BACKUP ]; then
    git push $GIT_BACKUPS/$ETC_GIT_BACKUP
#    echo -n "$0: enter directory "
#    pushd $ETC_GIT_BACKUP
#    git fetch -v /etc 
#    git gc
#    echo -n "$0: exit directory "
#    popd
else
    echo -n "$0: enter directory "
    pushd $GIT_BACKUPS
#    git clone -v --no-checkout /etc .
    git clone --bare /etc $ETC_GIT_BACKUP
    echo -n "$0: exit directory "
    popd
    chmod 755 $GIT_BACKUPS
    chmod 755 $GIT_BACKUPS/$ETC_GIT_BACKUP
fi
echo "$0: ---------------------------------------------------------"
echo " "

