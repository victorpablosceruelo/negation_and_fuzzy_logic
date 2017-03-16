#!/bin/sh
##
## procexists.sh
## 
## Made by Edison Mera
## Login   <edison@clip.dia.fi.upm.es>
## 
## Started on  Fri Jan 19 15:42:21 2007 Edison Mera
## Last update Mon Jul 13 17:39:36 2009 Edison Mera
##

# IMPORTANT:
# ==========

# In DARWIN there is not a /proc directory to see the pids of process,
# and in Windows there is not a fully functional ps command, in
# particular, the option -o is missing, so we need to use this
# platform independent script to see if a process is active or not.

if [ ! x$# = x1 ] ; then
    echo "Error: $0 Must have exactly one argument."
    exit 1
fi

if [ -d /proc ] ; then
    if [ -d /proc/$1 ] ; then
	exit 0;
    else
	exit 1;
    fi
else
    if [ `ps -p $1 -o pid=`x == x ] ; then
	exit 1;
    else
	exit 0;
    fi
fi
