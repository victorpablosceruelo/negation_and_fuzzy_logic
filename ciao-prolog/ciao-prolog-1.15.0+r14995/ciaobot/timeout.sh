#!/bin/bash
##
## timeout.sh
## 
## Made by Edison Mera
## Login   <edison@clip.dia.fi.upm.es>
## 
## Started on  Sat Jan  6 02:05:03 2007 Edison Mera
## Last update Mon Jul 27 18:45:06 2009 Edison Mera
##

# Note: To run this script, expect is required

# how long should I wait ?

cmd_timeout=$1

# Keyword that indicates that the command has finished
keyword=end_of_timeout

shift

# change this to the actual command that needs to run.
cmd=$*

# taken from: http://www.megalinux.net/archives/637.html

TIMEOUT_BASE=/tmp/timeout_${PPID}

# dont edit this
cmd_status=`expect <<EOF
set timeout ${cmd_timeout}
spawn /bin/bash
sleep 0.5
send -- "((${cmd}) 1>${TIMEOUT_BASE}.out 2>${TIMEOUT_BASE}.err; echo \$\?>${TIMEOUT_BASE}.tmp; echo ${keyword})\n"
sleep 0.5
expect {
"${keyword}" {exit 0}
timeout {echo 124>${TIMEOUT_BASE}.tmp; exit 124}
}
EOF`

cat ${TIMEOUT_BASE}.err > /dev/stderr
cat ${TIMEOUT_BASE}.out
ERRVAL=`cat ${TIMEOUT_BASE}.tmp`

rm -f ${TIMEOUT_BASE}.{err,log,tmp}

exit $ERRVAL
