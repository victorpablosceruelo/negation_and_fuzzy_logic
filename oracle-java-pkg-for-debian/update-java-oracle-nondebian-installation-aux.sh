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

INSTALLATION_PATH="${1}"
JAVA_JDK_PATH=
JAVA_LINK_1="java_jre"
JAVA_LINK_2="java_jdk"


if [ "`uname -m`" == "x86_64" ]; then 
    ARCH_FOLDER="amd64"
else
    ARCH_FOLDER="i386"
fi

# Syntax:
# update-alternatives --install link name path priority [--slave link name path]...

REMOVE="update-alternatives --remove-all"
INSTALL="update-alternatives --install"
UPDATE="update-alternatives --auto"
Priority="100"

# $0 -> function name
# $1 -> link
# $2 -> name
# $3 -> path
function real_install () {

    echo "real_install ${1} ${2} ${3} "
    
    ${REMOVE} ${2}
    ${INSTALL} ${1} ${2} ${3} ${Priority}
    ${UPDATE} ${2}

    echo " "
}

# In previous versions we used /usr/lib/java_installation
# We prefer not using it anymore and keeping everything in /opt folder.


real_install ${JAVA_JDK_PATH} java_installation ${INSTALLATION_PATH}/ ${Priority}
${UPDATE}  java_installation
update-alternatives --install /usr/bin/appletviewer appletviewer ${JAVA_JDK_PATH}/bin/appletviewer ${Priority} --slave /usr/share/man/man1/appletviewer.1 appletviewer.1 ${JAVA_JDK_PATH}/man/ja_JP.UTF-8/man1/appletviewer.1
update-alternatives --auto appletviewer
update-alternatives --install /usr/bin/apt apt ${JAVA_JDK_PATH}/bin/apt ${Priority} --slave /usr/lib/jvm/java-6-sun/man/man1/apt.1 apt.1 ${JAVA_JDK_PATH}/man/man1/apt.1
update-alternatives --auto apt
update-alternatives --install /usr/bin/ControlPanel ControlPanel ${JAVA_JDK_PATH}/bin/ControlPanel ${Priority}
update-alternatives --auto ControlPanel
update-alternatives --install /usr/bin/extcheck extcheck ${JAVA_JDK_PATH}/bin/extcheck ${Priority} --slave /usr/share/man/man1/extcheck.1 extcheck.1  ${JAVA_JDK_PATH}/man/man1/extcheck.1
update-alternatives --auto extcheck
update-alternatives --install /usr/lib/firefox/plugins/libjavaplugin.so firefox-javaplugin.so ${JAVA_JDK_PATH}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto firefox-javaplugin.so
# Correct place : jre/lib/amd64/libnpjp2.so
# update-alternatives --install /usr/bin/HtmlConverter HtmlConverter ???
update-alternatives --install /usr/lib/iceape/plugins/libjavaplugin.so iceape-javaplugin.so ${JAVA_JDK_PATH}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto iceape-javaplugin.so
update-alternatives --install /usr/lib/iceweasel/plugins/libjavaplugin.so iceweasel-javaplugin.so ${JAVA_JDK_PATH}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto iceweasel-javaplugin.so 
update-alternatives --install /usr/bin/idlj idlj  ${JAVA_JDK_PATH}/bin/idlj ${Priority} --slave /usr/share/man/man1/idlj.1 idlj.1 ${JAVA_JDK_PATH}/man/man1/idlj.1
update-alternatives --auto idlj 
# update-alternatives idlj.1.gz
update-alternatives --install /usr/bin/jar jar ${JAVA_JDK_PATH}/bin/jar ${Priority} --slave /usr/share/man/man1/jar.1 jar.1 ${JAVA_JDK_PATH}/man/man1/jar.1
update-alternatives --auto jar
# update-alternatives jar.1.gz
update-alternatives --install /usr/bin/jarsigner jarsigner ${JAVA_JDK_PATH}/bin/jarsigner ${Priority} --slave /usr/share/man/man1/jarsigner.1 jarsigner.1 ${JAVA_JDK_PATH}/man/man1/jarsigner.1
update-alternatives --auto jarsigner
# update-alternatives --install jarsigner.1.gz
update-alternatives --install /usr/bin/java java ${JAVA_JDK_PATH}/bin/java ${Priority}
update-alternatives --auto java
update-alternatives --install /usr/bin/javac javac ${JAVA_JDK_PATH}/bin/javac ${Priority} --slave /usr/share/man/man1/javac.1 javac.1 ${JAVA_JDK_PATH}/man/man1/javac.1
update-alternatives --auto javac
# update-alternatives --install javac.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javac.1.gz
update-alternatives --install /usr/bin/javadoc javadoc ${JAVA_JDK_PATH}/bin/javadoc ${Priority} --slave /usr/share/man/man1/javadoc.1 javadoc.1 ${JAVA_JDK_PATH}/man/man1/javadoc.1
update-alternatives --auto javadoc
# update-alternatives --install javadoc.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javadoc.1.gz
update-alternatives --install /usr/bin/javah javah ${JAVA_JDK_PATH}/bin/javah ${Priority} --slave /usr/share/man/man1/javah.1 javah.1 ${JAVA_JDK_PATH}/man/man1/javah.1
update-alternatives --auto javah
# update-alternatives --install javah.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javah.1.gz
update-alternatives --install /usr/bin/javap javap ${JAVA_JDK_PATH}/bin/javap ${Priority} --slave /usr/share/man/man1/javap.1 javap.1 ${JAVA_JDK_PATH}/man/man1/javap.1
update-alternatives --auto javap
# update-alternatives --install javap.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javap.1.gz
update-alternatives --install /usr/bin/java_vm java_vm ${JAVA_JDK_PATH}/jre/bin/java_vm ${Priority}
update-alternatives --auto java_vm
update-alternatives --install /usr/bin/javaws javaws ${JAVA_JDK_PATH}/bin/javaws ${Priority} --slave /usr/share/man/man1/javaws.1 javaws.1 ${JAVA_JDK_PATH}/man/man1/javaws.1
update-alternatives --auto javaws
# update-alternatives --install javaws.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/javaws.1.gz
update-alternatives --install /usr/bin/jconsole jconsole ${JAVA_JDK_PATH}/bin/jconsole ${Priority} --slave /usr/share/man/man1/jconsole.1 jconsole.1 ${JAVA_JDK_PATH}/man/man1/jconsole.1
update-alternatives --auto jconsole
# update-alternatives --install jconsole.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jconsole.1.gz
update-alternatives --install /usr/bin/jcontrol jcontrol ${JAVA_JDK_PATH}/bin/jcontrol ${Priority}
update-alternatives --auto jcontrol
update-alternatives --install /usr/bin/jdb jdb ${JAVA_JDK_PATH}/bin/jdb ${Priority} --slave /usr/share/man/man1/jdb.1 jdb.1 ${JAVA_JDK_PATH}/man/man1/jdb.1
update-alternatives --auto jdb
# update-alternatives --install jdb.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jdb.1.gz
update-alternatives --install /usr/bin/jexec jexec ${JAVA_JDK_PATH}/lib/jexec ${Priority}
update-alternatives --auto jexec
# ${Priority} --slave /usr/share/binfmts/jar jexec-binfmt 
# update-alternatives --install jexec-binfmt -> /usr/lib/jvm/java-6-sun/jre/lib/jar.binfmt
update-alternatives --install /usr/bin/jhat jhat ${JAVA_JDK_PATH}/bin/jhat ${Priority} --slave /usr/share/man/man1/jhat.1 jhat.1 ${JAVA_JDK_PATH}/man/man1/jhat.1
update-alternatives --auto jhat
# update-alternatives --install jhat.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jhat.1.gz
update-alternatives --install /usr/bin/jinfo jinfo ${JAVA_JDK_PATH}/bin/jinfo ${Priority} --slave /usr/share/man/man1/jinfo.1 jinfo.1 ${JAVA_JDK_PATH}/man/man1/jinfo.1
update-alternatives --auto jinfo 
# update-alternatives --install jinfo.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jinfo.1.gz
update-alternatives --install /usr/bin/jmap jmap ${JAVA_JDK_PATH}/bin/jmap ${Priority} --slave /usr/share/man/man1/jmap.1 jmap.1 ${JAVA_JDK_PATH}/man/man1/jmap.1
update-alternatives --auto jmap
# update-alternatives --install jmap.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jmap.1.gz
update-alternatives --install /usr/bin/jps jps ${JAVA_JDK_PATH}/bin/jps ${Priority} --slave /usr/share/man/man1/jps.1 jps.1 ${JAVA_JDK_PATH}/man/man1/jps.1
update-alternatives --auto jps
# update-alternatives --install jps.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jps.1.gz
update-alternatives --install /usr/bin/jrunscript jrunscript ${JAVA_JDK_PATH}/bin/jrunscript ${Priority} --slave /usr/share/man/man1/jrunscript.1 jrunscript.1  ${JAVA_JDK_PATH}/man/man1/jrunscript.1
update-alternatives --auto jrunscript
# update-alternatives --install jrunscript.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jrunscript.1.gz
update-alternatives --install /usr/bin/jsadebugd jsadebugd ${JAVA_JDK_PATH}/bin/jsadebugd ${Priority} --slave /usr/share/man/man1/jsadebugd.1 jsadebugd.1 ${JAVA_JDK_PATH}/man/man1/jsadebugd.1
update-alternatives --auto jsadebugd
# update-alternatives --install jsadebugd.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jsadebugd.1.gz
update-alternatives --install /usr/bin/jstack jstack ${JAVA_JDK_PATH}/bin/jstack ${Priority} --slave /usr/share/man/man1/jstack.1 jstack.1 ${JAVA_JDK_PATH}/man/man1/jstack.1
update-alternatives --auto jstack
# update-alternatives --install jstack.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstack.1.gz
update-alternatives --install /usr/bin/jstat jstat ${JAVA_JDK_PATH}/bin/jstat ${Priority} --slave /usr/share/man/man1/jstat.1 jstat.1 ${JAVA_JDK_PATH}/man/man1/jstat.1
update-alternatives --auto jstat
# update-alternatives --install jstat.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstat.1.gz
update-alternatives --install /usr/bin/jstatd jstatd ${JAVA_JDK_PATH}/bin/jstatd ${Priority} --slave /usr/share/man/man1/jstatd.1 jstatd.1 ${JAVA_JDK_PATH}/man/man1/jstatd.1
update-alternatives --auto jstatd
# update-alternatives --install jstatd.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstatd.1.gz
update-alternatives --install /usr/bin/keytool keytool ${JAVA_JDK_PATH}/bin/keytool ${Priority} --slave /usr/share/man/man1/keytool.1 keytool.1 ${JAVA_JDK_PATH}/man/man1/keytool.1
update-alternatives --auto keytool
# update-alternatives --install keytool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/keytool.1.gz
update-alternatives --install /usr/lib/midbrowser/plugins/libjavaplugin.so midbrowser-javaplugin.so ${JAVA_JDK_PATH}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto midbrowser-javaplugin.so
update-alternatives --install /usr/lib/mozilla/plugins/libjavaplugin.so mozilla-javaplugin.so ${JAVA_JDK_PATH}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto mozilla-javaplugin.so
update-alternatives --install /usr/bin/native2ascii native2ascii ${JAVA_JDK_PATH}/bin/native2ascii ${Priority} --slave /usr/share/man/man1/native2ascii.1 native2ascii.1 ${JAVA_JDK_PATH}/man/man1/native2ascii.1
update-alternatives --auto native2ascii
# update-alternatives --install native2ascii.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/native2ascii.1.gz
update-alternatives --install /usr/bin/orbd orbd ${JAVA_JDK_PATH}/bin/orbd ${Priority} --slave /usr/share/man/man1/orbd.1 orbd.1 ${JAVA_JDK_PATH}/man/man1/orbd.1
update-alternatives --auto orbd
# update-alternatives --install orbd.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/orbd.1.gz
update-alternatives --install /usr/bin/pack200 pack200 ${JAVA_JDK_PATH}/bin/pack200 ${Priority} --slave /usr/share/man/man1/pack200.1 pack200.1 ${JAVA_JDK_PATH}/man/man1/pack200.1
update-alternatives --auto pack200
# update-alternatives --install pack200.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/pack200.1.gz
update-alternatives --install /usr/bin/policytool policytool ${JAVA_JDK_PATH}/bin/policytool ${Priority} --slave /usr/share/man/man1/policytool.1 policytool.1 ${JAVA_JDK_PATH}/man/man1/policytool.1
update-alternatives --auto policytool
#update-alternatives --install policytool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/policytool.1.gz
update-alternatives --install /usr/bin/rmic rmic ${JAVA_JDK_PATH}/bin/rmic ${Priority} --slave /usr/share/man/man1/rmic.1 rmic.1 ${JAVA_JDK_PATH}/man/man1/rmic.1
update-alternatives --auto rmic
# update-alternatives --install rmic.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/rmic.1.gz
update-alternatives --install /usr/bin/rmid rmid ${JAVA_JDK_PATH}/bin/rmid ${Priority} --slave /usr/share/man/man1/rmid.1 rmid.1  ${JAVA_JDK_PATH}/man/man1/rmid.1
update-alternatives --auto rmid
# update-alternatives --install rmid.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/rmid.1.gz
update-alternatives --install /usr/bin/rmiregistry rmiregistry ${JAVA_JDK_PATH}/bin/rmiregistry ${Priority} --slave /usr/share/man/man1/rmiregistry.1 rmiregistry.1 ${JAVA_JDK_PATH}/man/man1/rmiregistry.1
update-alternatives --auto rmiregistry
#update-alternatives --install rmiregistry.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/rmiregistry.1.gz
update-alternatives --install /usr/bin/schemagen schemagen ${JAVA_JDK_PATH}/bin/schemagen ${Priority} --slave /usr/share/man/man1/schemagen.1 schemagen.1 ${JAVA_JDK_PATH}/man/man1/schemagen.1
update-alternatives --auto schemagen
# update-alternatives --install schemagen.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/schemagen.1.gz
update-alternatives --install /usr/bin/serialver serialver ${JAVA_JDK_PATH}/bin/serialver ${Priority} --slave /usr/share/man/man1/serialver.1 serialver.1 ${JAVA_JDK_PATH}/man/man1/serialver.1
update-alternatives --auto serialver
# update-alternatives --install serialver.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/serialver.1.gz
update-alternatives --install /usr/bin/servertool servertool ${JAVA_JDK_PATH}/bin/servertool ${Priority} --slave /usr/share/man/man1/servertool.1 servertool.1 ${JAVA_JDK_PATH}/man/man1/servertool.1
update-alternatives --auto servertool
# update-alternatives --install servertool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/servertool.1.gz
update-alternatives --install /usr/bin/tnameserv tnameserv ${JAVA_JDK_PATH}/bin/tnameserv ${Priority} --slave /usr/share/man/man1/tnameserv.1 tnameserv.1 ${JAVA_JDK_PATH}/man/man1/tnameserv.1
update-alternatives --auto tnameserv
# update-alternatives --install tnameserv.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/tnameserv.1.gz
update-alternatives --install /usr/bin/unpack200 unpack200 ${JAVA_JDK_PATH}/bin/unpack200 ${Priority} --slave /usr/share/man/man1/unpack200.1 unpack200.1 ${JAVA_JDK_PATH}/man/man1/unpack200.1
update-alternatives --auto unpack200
#update-alternatives --install unpack200.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/unpack200.1.gz
update-alternatives --install /usr/bin/wsgen wsgen ${JAVA_JDK_PATH}/bin/wsgen ${Priority} --slave  /usr/share/man/man1/wsgen.1 wsgen.1 ${JAVA_JDK_PATH}/man/man1/wsgen.1
update-alternatives --auto wsgen
# update-alternatives --install wsgen.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/wsgen.1.gz
update-alternatives --install /usr/bin/wsimport wsimport ${JAVA_JDK_PATH}/bin/wsimport ${Priority} --slave /usr/share/man/man1/wsimport.1 wsimport.1 ${JAVA_JDK_PATH}/man/man1/wsimport.1
update-alternatives --auto wsimport
# update-alternatives --install wsimport.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/wsimport.1.gz
update-alternatives --install /usr/bin/xjc xjc ${JAVA_JDK_PATH}/bin/xjc ${Priority} --slave /usr/share/man/man1/xjc.1 xjc.1 ${JAVA_JDK_PATH}/man/man1/xjc.1
update-alternatives --auto xjc
# update-alternatives --install xjc.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/xjc.1.gz
update-alternatives --install /usr/lib/xulrunner-addons/plugins/libjavaplugin.so xulrunner-1.9-javaplugin.so ${JAVA_JDK_PATH}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto xulrunner-1.9-javaplugin.so
update-alternatives --install /usr/lib/xulrunner/plugins/libjavaplugin.so xulrunner-javaplugin.so ${JAVA_JDK_PATH}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto xulrunner-javaplugin.so

update-alternatives --install /usr/lib/iceweasel/plugins/libjavaplugin.so libjavaplugin.so ${JAVA_JDK_PATH}/jre/lib/amd64/libnpjp2.so ${Priority}
update-alternatives --auto libjavaplugin.so

update-alternatives --install /usr/lib/iceweasel/plugins/libnpjp2.so libnpjp2.so ${JAVA_JDK_PATH}/jre/lib/amd64/libnpjp2.so ${Priority}
update-alternatives --auto libnpjp2.so

# From Sara's computer (i386).
update-alternatives --install /usr/lib/jvm/java-6-sun/bin/jvisualvm jvisualvm ${JAVA_JDK_PATH}/bin/jvisualvm ${Priority}
update-alternatives --auto jvisualvm

#EOF

