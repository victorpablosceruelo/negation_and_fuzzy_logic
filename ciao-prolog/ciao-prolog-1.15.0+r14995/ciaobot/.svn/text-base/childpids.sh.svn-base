#!/bin/sh
##
## childpids.sh
## 
## Made by Edison Mera
## Login   <edison@clip.dia.fi.upm.es>
## 
## Started on  Sun Apr 11 13:27:49 2009 Edison Mera
## Last update Tue Jul 14 00:50:30 2009 Edison Mera
##

set -e

if [ $# = 0 ] ; then
    echo "List recursively the children process id of a given parent process"
    echo
    echo "Usage:"
    echo $0 "ppid"
    echo
    echo "Where ppid is the parent process id"
exit 1
fi

# get_ppid obtain the list of process ids for a given parent process
# id.  It is platform dependent, due to ps varies in each one
get_ppid() {
    if [ -f ./ciao_get_arch ] ; then
	case `./ciao_get_arch` in
	    Solaris*|DARWIN*) ps -e -o pid= -o ppid=|grep " $1\$"|sed -e s/" $1"//g ;;
	    Win*)             ps -e|awk '{ print $1 " " $2 }'|grep " $1\$"|sed -e s/" $1"//g ;;
	    *)                ps --ppid "$1" -o pid= ;;
	esac
    else
	ps --ppid "$1" -o pid=
    fi
}

get_childpids() {
    for pid in `get_ppid $1`
    do
	get_childpids $pid
    done
    echo $1
}

get_childpids $1
