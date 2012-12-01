#! /bin/bash

if [ -z "$1" ] || [ "$1" == "" ] || [ -z "$2" ] || [ "$2" == "" ]; then
	echo "usage: $0 java-installation-path java-installation-subpath"
	echo "example: $0 /opt/oracle-java oracle-java-subfolder "
	echo "you need super-user rights to run this script."
	exit 0
fi

if [ ! -d "${1}" ]; then
    echo "ERROR: folder ${1} does not exist. "
    exit 0
fi

if [ ! -d "${1}/${2}" ]; then
    echo "ERROR: folder ${2} does not exist. "
    exit 0
fi

OLD_INSTALLATION="/usr/lib/java_installation"
if [ -d ${OLD_INSTALLATION} ] || [ -f ${OLD_INSTALLATION} ]; then
    echo "In previous versions we used the path ${OLD_INSTALLATION} to point to the installation folder."
    echo "We do not use it anymore."
    echo "Answer yes if you want to remove it."
    rm -iv ${OLD_INSTALLATION}
fi

INSTALLATION_PATH="${1}/${2}"
JAVA_INSTALLATION="${1}/java_installation"
JAVA_JDK_PATH="${1}/java_jdk"
JAVA_JRE_PATH="${1}/java_jre"

if [ "`uname -m`" == "x86_64" ]; then 
    ARCH_FOLDER="amd64"
else
    ARCH_FOLDER="i386"
fi


# $0 -> function name
# $1 -> link
# $2 -> name
# $3 -> path
function real_install () {

    if [ ! -z "${3}" ] && [ ! "" == "${3}" ]; then 
	NEW_3="${JAVA_JDK_PATH}/${3}"
    fi

    if [ ! -z "${7}" ] && [ ! "" == "${7}" ]; then 
	NEW_7="${JAVA_JDK_PATH}/${7}"
    fi
    
    real_install_aux ${1} ${2} ${NEW_3} ${4} ${5} ${6} ${NEW_7}
}

function real_install_aux () {
    Priority="100"    
    echo "real_install ${1} ${2} ${3} ${Priority} ${4} ${5} ${6} ${7}"

    # Syntax:
    # update-alternatives --install link name path priority [--slave link name path]...

    update-alternatives --remove-all ${2}
    update-alternatives --install ${1} ${2} ${3} ${Priority} ${4} ${5} ${6} ${7}
    update-alternatives --auto ${2}

    echo " "
}

# In previous versions we used /usr/lib/java_installation
# We prefer not using it anymore and keeping everything in /opt folder.


real_install_aux ${JAVA_INSTALLATION} java_installation ${INSTALLATION_PATH} 
real_install_aux ${JAVA_JDK_PATH} java_jdk ${INSTALLATION_PATH} 
real_install_aux ${JAVA_JRE_PATH} java_jre ${INSTALLATION_PATH} 

real_install /usr/bin/appletviewer appletviewer bin/appletviewer  --slave /usr/share/man/man1/appletviewer.1 appletviewer.1 man/ja_JP.UTF-8/man1/appletviewer.1
update-alternatives --auto appletviewer
real_install /usr/bin/apt apt bin/apt  --slave /usr/lib/jvm/java-6-sun/man/man1/apt.1 apt.1 man/man1/apt.1
update-alternatives --auto apt
real_install /usr/bin/ControlPanel ControlPanel bin/ControlPanel 
update-alternatives --auto ControlPanel
real_install /usr/bin/extcheck extcheck bin/extcheck  --slave /usr/share/man/man1/extcheck.1 extcheck.1  man/man1/extcheck.1
update-alternatives --auto extcheck
real_install /usr/lib/firefox/plugins/libjavaplugin.so firefox-javaplugin.so jre/lib/${ARCH_FOLDER}/libnpjp2.so 
update-alternatives --auto firefox-javaplugin.so
# Correct place : jre/lib/amd64/libnpjp2.so
# real_install /usr/bin/HtmlConverter HtmlConverter ???
real_install /usr/lib/iceape/plugins/libjavaplugin.so iceape-javaplugin.so jre/lib/${ARCH_FOLDER}/libnpjp2.so 
update-alternatives --auto iceape-javaplugin.so
real_install /usr/lib/iceweasel/plugins/libjavaplugin.so iceweasel-javaplugin.so jre/lib/${ARCH_FOLDER}/libnpjp2.so 
update-alternatives --auto iceweasel-javaplugin.so 
real_install /usr/bin/idlj idlj  bin/idlj  --slave /usr/share/man/man1/idlj.1 idlj.1 man/man1/idlj.1
update-alternatives --auto idlj 
# update-alternatives idlj.1.gz
real_install /usr/bin/jar jar bin/jar  --slave /usr/share/man/man1/jar.1 jar.1 man/man1/jar.1
update-alternatives --auto jar
# update-alternatives jar.1.gz
real_install /usr/bin/jarsigner jarsigner bin/jarsigner  --slave /usr/share/man/man1/jarsigner.1 jarsigner.1 man/man1/jarsigner.1
update-alternatives --auto jarsigner
# real_install jarsigner.1.gz
real_install /usr/bin/java java bin/java 
update-alternatives --auto java
real_install /usr/bin/javac javac bin/javac  --slave /usr/share/man/man1/javac.1 javac.1 man/man1/javac.1
update-alternatives --auto javac
# real_install javac.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javac.1.gz
real_install /usr/bin/javadoc javadoc bin/javadoc  --slave /usr/share/man/man1/javadoc.1 javadoc.1 man/man1/javadoc.1
update-alternatives --auto javadoc
# real_install javadoc.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javadoc.1.gz
real_install /usr/bin/javah javah bin/javah  --slave /usr/share/man/man1/javah.1 javah.1 man/man1/javah.1
update-alternatives --auto javah
# real_install javah.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javah.1.gz
real_install /usr/bin/javap javap bin/javap  --slave /usr/share/man/man1/javap.1 javap.1 man/man1/javap.1
update-alternatives --auto javap
# real_install javap.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javap.1.gz
real_install /usr/bin/java_vm java_vm jre/bin/java_vm 
update-alternatives --auto java_vm
real_install /usr/bin/javaws javaws bin/javaws  --slave /usr/share/man/man1/javaws.1 javaws.1 man/man1/javaws.1
update-alternatives --auto javaws
# real_install javaws.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/javaws.1.gz
real_install /usr/bin/jconsole jconsole bin/jconsole  --slave /usr/share/man/man1/jconsole.1 jconsole.1 man/man1/jconsole.1
update-alternatives --auto jconsole
# real_install jconsole.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jconsole.1.gz
real_install /usr/bin/jcontrol jcontrol bin/jcontrol 
update-alternatives --auto jcontrol
real_install /usr/bin/jdb jdb bin/jdb  --slave /usr/share/man/man1/jdb.1 jdb.1 man/man1/jdb.1
update-alternatives --auto jdb
# real_install jdb.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jdb.1.gz
real_install /usr/bin/jexec jexec lib/jexec 
update-alternatives --auto jexec
#  --slave /usr/share/binfmts/jar jexec-binfmt 
# real_install jexec-binfmt -> /usr/lib/jvm/java-6-sun/jre/lib/jar.binfmt
real_install /usr/bin/jhat jhat bin/jhat  --slave /usr/share/man/man1/jhat.1 jhat.1 man/man1/jhat.1
update-alternatives --auto jhat
# real_install jhat.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jhat.1.gz
real_install /usr/bin/jinfo jinfo bin/jinfo  --slave /usr/share/man/man1/jinfo.1 jinfo.1 man/man1/jinfo.1
update-alternatives --auto jinfo 
# real_install jinfo.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jinfo.1.gz
real_install /usr/bin/jmap jmap bin/jmap  --slave /usr/share/man/man1/jmap.1 jmap.1 man/man1/jmap.1
update-alternatives --auto jmap
# real_install jmap.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jmap.1.gz
real_install /usr/bin/jps jps bin/jps  --slave /usr/share/man/man1/jps.1 jps.1 man/man1/jps.1
update-alternatives --auto jps
# real_install jps.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jps.1.gz
real_install /usr/bin/jrunscript jrunscript bin/jrunscript  --slave /usr/share/man/man1/jrunscript.1 jrunscript.1  man/man1/jrunscript.1
update-alternatives --auto jrunscript
# real_install jrunscript.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jrunscript.1.gz
real_install /usr/bin/jsadebugd jsadebugd bin/jsadebugd  --slave /usr/share/man/man1/jsadebugd.1 jsadebugd.1 man/man1/jsadebugd.1
update-alternatives --auto jsadebugd
# real_install jsadebugd.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jsadebugd.1.gz
real_install /usr/bin/jstack jstack bin/jstack  --slave /usr/share/man/man1/jstack.1 jstack.1 man/man1/jstack.1
update-alternatives --auto jstack
# real_install jstack.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstack.1.gz
real_install /usr/bin/jstat jstat bin/jstat  --slave /usr/share/man/man1/jstat.1 jstat.1 man/man1/jstat.1
update-alternatives --auto jstat
# real_install jstat.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstat.1.gz
real_install /usr/bin/jstatd jstatd bin/jstatd  --slave /usr/share/man/man1/jstatd.1 jstatd.1 man/man1/jstatd.1
update-alternatives --auto jstatd
# real_install jstatd.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstatd.1.gz
real_install /usr/bin/keytool keytool bin/keytool  --slave /usr/share/man/man1/keytool.1 keytool.1 man/man1/keytool.1
update-alternatives --auto keytool
# real_install keytool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/keytool.1.gz
real_install /usr/lib/midbrowser/plugins/libjavaplugin.so midbrowser-javaplugin.so jre/lib/${ARCH_FOLDER}/libnpjp2.so 
update-alternatives --auto midbrowser-javaplugin.so
real_install /usr/lib/mozilla/plugins/libjavaplugin.so mozilla-javaplugin.so jre/lib/${ARCH_FOLDER}/libnpjp2.so 
update-alternatives --auto mozilla-javaplugin.so
real_install /usr/bin/native2ascii native2ascii bin/native2ascii  --slave /usr/share/man/man1/native2ascii.1 native2ascii.1 man/man1/native2ascii.1
update-alternatives --auto native2ascii
# real_install native2ascii.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/native2ascii.1.gz
real_install /usr/bin/orbd orbd bin/orbd  --slave /usr/share/man/man1/orbd.1 orbd.1 man/man1/orbd.1
update-alternatives --auto orbd
# real_install orbd.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/orbd.1.gz
real_install /usr/bin/pack200 pack200 bin/pack200  --slave /usr/share/man/man1/pack200.1 pack200.1 man/man1/pack200.1
update-alternatives --auto pack200
# real_install pack200.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/pack200.1.gz
real_install /usr/bin/policytool policytool bin/policytool  --slave /usr/share/man/man1/policytool.1 policytool.1 man/man1/policytool.1
update-alternatives --auto policytool
#real_install policytool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/policytool.1.gz
real_install /usr/bin/rmic rmic bin/rmic  --slave /usr/share/man/man1/rmic.1 rmic.1 man/man1/rmic.1
update-alternatives --auto rmic
# real_install rmic.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/rmic.1.gz
real_install /usr/bin/rmid rmid bin/rmid  --slave /usr/share/man/man1/rmid.1 rmid.1  man/man1/rmid.1
update-alternatives --auto rmid
# real_install rmid.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/rmid.1.gz
real_install /usr/bin/rmiregistry rmiregistry bin/rmiregistry  --slave /usr/share/man/man1/rmiregistry.1 rmiregistry.1 man/man1/rmiregistry.1
update-alternatives --auto rmiregistry
#real_install rmiregistry.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/rmiregistry.1.gz
real_install /usr/bin/schemagen schemagen bin/schemagen  --slave /usr/share/man/man1/schemagen.1 schemagen.1 man/man1/schemagen.1
update-alternatives --auto schemagen
# real_install schemagen.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/schemagen.1.gz
real_install /usr/bin/serialver serialver bin/serialver  --slave /usr/share/man/man1/serialver.1 serialver.1 man/man1/serialver.1
update-alternatives --auto serialver
# real_install serialver.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/serialver.1.gz
real_install /usr/bin/servertool servertool bin/servertool  --slave /usr/share/man/man1/servertool.1 servertool.1 man/man1/servertool.1
update-alternatives --auto servertool
# real_install servertool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/servertool.1.gz
real_install /usr/bin/tnameserv tnameserv bin/tnameserv  --slave /usr/share/man/man1/tnameserv.1 tnameserv.1 man/man1/tnameserv.1
update-alternatives --auto tnameserv
# real_install tnameserv.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/tnameserv.1.gz
real_install /usr/bin/unpack200 unpack200 bin/unpack200  --slave /usr/share/man/man1/unpack200.1 unpack200.1 man/man1/unpack200.1
update-alternatives --auto unpack200
#real_install unpack200.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/unpack200.1.gz
real_install /usr/bin/wsgen wsgen bin/wsgen  --slave  /usr/share/man/man1/wsgen.1 wsgen.1 man/man1/wsgen.1
update-alternatives --auto wsgen
# real_install wsgen.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/wsgen.1.gz
real_install /usr/bin/wsimport wsimport bin/wsimport  --slave /usr/share/man/man1/wsimport.1 wsimport.1 man/man1/wsimport.1
update-alternatives --auto wsimport
# real_install wsimport.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/wsimport.1.gz
real_install /usr/bin/xjc xjc bin/xjc  --slave /usr/share/man/man1/xjc.1 xjc.1 man/man1/xjc.1
update-alternatives --auto xjc
# real_install xjc.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/xjc.1.gz
real_install /usr/lib/xulrunner-addons/plugins/libjavaplugin.so xulrunner-1.9-javaplugin.so jre/lib/${ARCH_FOLDER}/libnpjp2.so 
update-alternatives --auto xulrunner-1.9-javaplugin.so
real_install /usr/lib/xulrunner/plugins/libjavaplugin.so xulrunner-javaplugin.so jre/lib/${ARCH_FOLDER}/libnpjp2.so 
update-alternatives --auto xulrunner-javaplugin.so

real_install /usr/lib/iceweasel/plugins/libjavaplugin.so libjavaplugin.so jre/lib/amd64/libnpjp2.so 
update-alternatives --auto libjavaplugin.so

real_install /usr/lib/iceweasel/plugins/libnpjp2.so libnpjp2.so jre/lib/amd64/libnpjp2.so 
update-alternatives --auto libnpjp2.so

# From Sara's computer (i386).
real_install /usr/lib/jvm/java-6-sun/bin/jvisualvm jvisualvm bin/jvisualvm 
update-alternatives --auto jvisualvm

#EOF

