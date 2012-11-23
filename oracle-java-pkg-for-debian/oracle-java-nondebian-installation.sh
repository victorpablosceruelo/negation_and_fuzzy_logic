#! /bin/sh

if [ -z "$1" ] || [ "$1" == "" ]; then
	echo "usage: $0 java-installation-path "
	echo "example: $0 /opt/java_jre_oracle "
	echo "you need super-user rights to run this script."
	exit 0
fi

if [ ! -d "$1" ]; then
	if [ ! -f "$1" ]; then
		echo "WARNING: folder does not exist. "
		exit 0
	fi
fi

RealPath="${1}"
FakePath="/usr/lib/java_installation"
Priority="100"
CMD="update-alternatives"

if [ "`uname -m`" == "x86_64" ]; then 
    ARCH_FOLDER="amd64"
else
    ARCH_FOLDER="i386"
fi

# Syntax:
# update-alternatives --install link name path priority [--slave link name path]...

update-alternatives --install ${FakePath} java_installation ${RealPath}/ ${Priority}
update-alternatives --auto java_installation
update-alternatives --install /usr/bin/appletviewer appletviewer ${FakePath}/bin/appletviewer ${Priority} --slave /usr/share/man/man1/appletviewer.1 appletviewer.1 ${FakePath}/man/ja_JP.UTF-8/man1/appletviewer.1
update-alternatives --auto appletviewer
update-alternatives --install /usr/bin/apt apt ${FakePath}/bin/apt ${Priority} --slave /usr/lib/jvm/java-6-sun/man/man1/apt.1 apt.1 ${FakePath}/man/man1/apt.1
update-alternatives --auto apt
update-alternatives --install /usr/bin/ControlPanel ControlPanel ${FakePath}/bin/ControlPanel ${Priority}
update-alternatives --auto ControlPanel
update-alternatives --install /usr/bin/extcheck extcheck ${FakePath}/bin/extcheck ${Priority} --slave /usr/share/man/man1/extcheck.1 extcheck.1  ${FakePath}/man/man1/extcheck.1
update-alternatives --auto extcheck
update-alternatives --install /usr/lib/firefox/plugins/libjavaplugin.so firefox-javaplugin.so ${FakePath}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto firefox-javaplugin.so
# Correct place : jre/lib/amd64/libnpjp2.so
# update-alternatives --install /usr/bin/HtmlConverter HtmlConverter ???
update-alternatives --install /usr/lib/iceape/plugins/libjavaplugin.so iceape-javaplugin.so ${FakePath}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto iceape-javaplugin.so
update-alternatives --install /usr/lib/iceweasel/plugins/libjavaplugin.so iceweasel-javaplugin.so ${FakePath}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto iceweasel-javaplugin.so 
update-alternatives --install /usr/bin/idlj idlj  ${FakePath}/bin/idlj ${Priority} --slave /usr/share/man/man1/idlj.1 idlj.1 ${FakePath}/man/man1/idlj.1
update-alternatives --auto idlj 
# update-alternatives idlj.1.gz
update-alternatives --install /usr/bin/jar jar ${FakePath}/bin/jar ${Priority} --slave /usr/share/man/man1/jar.1 jar.1 ${FakePath}/man/man1/jar.1
update-alternatives --auto jar
# update-alternatives jar.1.gz
update-alternatives --install /usr/bin/jarsigner jarsigner ${FakePath}/bin/jarsigner ${Priority} --slave /usr/share/man/man1/jarsigner.1 jarsigner.1 ${FakePath}/man/man1/jarsigner.1
update-alternatives --auto jarsigner
# update-alternatives --install jarsigner.1.gz
update-alternatives --install /usr/bin/java java ${FakePath}/bin/java ${Priority}
update-alternatives --auto java
update-alternatives --install /usr/bin/javac javac ${FakePath}/bin/javac ${Priority} --slave /usr/share/man/man1/javac.1 javac.1 ${FakePath}/man/man1/javac.1
update-alternatives --auto javac
# update-alternatives --install javac.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javac.1.gz
update-alternatives --install /usr/bin/javadoc javadoc ${FakePath}/bin/javadoc ${Priority} --slave /usr/share/man/man1/javadoc.1 javadoc.1 ${FakePath}/man/man1/javadoc.1
update-alternatives --auto javadoc
# update-alternatives --install javadoc.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javadoc.1.gz
update-alternatives --install /usr/bin/javah javah ${FakePath}/bin/javah ${Priority} --slave /usr/share/man/man1/javah.1 javah.1 ${FakePath}/man/man1/javah.1
update-alternatives --auto javah
# update-alternatives --install javah.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javah.1.gz
update-alternatives --install /usr/bin/javap javap ${FakePath}/bin/javap ${Priority} --slave /usr/share/man/man1/javap.1 javap.1 ${FakePath}/man/man1/javap.1
update-alternatives --auto javap
# update-alternatives --install javap.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/javap.1.gz
update-alternatives --install /usr/bin/java_vm java_vm ${FakePath}/jre/bin/java_vm ${Priority}
update-alternatives --auto java_vm
update-alternatives --install /usr/bin/javaws javaws ${FakePath}/bin/javaws ${Priority} --slave /usr/share/man/man1/javaws.1 javaws.1 ${FakePath}/man/man1/javaws.1
update-alternatives --auto javaws
# update-alternatives --install javaws.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/javaws.1.gz
update-alternatives --install /usr/bin/jconsole jconsole ${FakePath}/bin/jconsole ${Priority} --slave /usr/share/man/man1/jconsole.1 jconsole.1 ${FakePath}/man/man1/jconsole.1
update-alternatives --auto jconsole
# update-alternatives --install jconsole.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jconsole.1.gz
update-alternatives --install /usr/bin/jcontrol jcontrol ${FakePath}/bin/jcontrol ${Priority}
update-alternatives --auto jcontrol
update-alternatives --install /usr/bin/jdb jdb ${FakePath}/bin/jdb ${Priority} --slave /usr/share/man/man1/jdb.1 jdb.1 ${FakePath}/man/man1/jdb.1
update-alternatives --auto jdb
# update-alternatives --install jdb.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jdb.1.gz
update-alternatives --install /usr/bin/jexec jexec ${FakePath}/lib/jexec ${Priority}
update-alternatives --auto jexec
# ${Priority} --slave /usr/share/binfmts/jar jexec-binfmt 
# update-alternatives --install jexec-binfmt -> /usr/lib/jvm/java-6-sun/jre/lib/jar.binfmt
update-alternatives --install /usr/bin/jhat jhat ${FakePath}/bin/jhat ${Priority} --slave /usr/share/man/man1/jhat.1 jhat.1 ${FakePath}/man/man1/jhat.1
update-alternatives --auto jhat
# update-alternatives --install jhat.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jhat.1.gz
update-alternatives --install /usr/bin/jinfo jinfo ${FakePath}/bin/jinfo ${Priority} --slave /usr/share/man/man1/jinfo.1 jinfo.1 ${FakePath}/man/man1/jinfo.1
update-alternatives --auto jinfo 
# update-alternatives --install jinfo.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jinfo.1.gz
update-alternatives --install /usr/bin/jmap jmap ${FakePath}/bin/jmap ${Priority} --slave /usr/share/man/man1/jmap.1 jmap.1 ${FakePath}/man/man1/jmap.1
update-alternatives --auto jmap
# update-alternatives --install jmap.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jmap.1.gz
update-alternatives --install /usr/bin/jps jps ${FakePath}/bin/jps ${Priority} --slave /usr/share/man/man1/jps.1 jps.1 ${FakePath}/man/man1/jps.1
update-alternatives --auto jps
# update-alternatives --install jps.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jps.1.gz
update-alternatives --install /usr/bin/jrunscript jrunscript ${FakePath}/bin/jrunscript ${Priority} --slave /usr/share/man/man1/jrunscript.1 jrunscript.1  ${FakePath}/man/man1/jrunscript.1
update-alternatives --auto jrunscript
# update-alternatives --install jrunscript.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jrunscript.1.gz
update-alternatives --install /usr/bin/jsadebugd jsadebugd ${FakePath}/bin/jsadebugd ${Priority} --slave /usr/share/man/man1/jsadebugd.1 jsadebugd.1 ${FakePath}/man/man1/jsadebugd.1
update-alternatives --auto jsadebugd
# update-alternatives --install jsadebugd.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jsadebugd.1.gz
update-alternatives --install /usr/bin/jstack jstack ${FakePath}/bin/jstack ${Priority} --slave /usr/share/man/man1/jstack.1 jstack.1 ${FakePath}/man/man1/jstack.1
update-alternatives --auto jstack
# update-alternatives --install jstack.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstack.1.gz
update-alternatives --install /usr/bin/jstat jstat ${FakePath}/bin/jstat ${Priority} --slave /usr/share/man/man1/jstat.1 jstat.1 ${FakePath}/man/man1/jstat.1
update-alternatives --auto jstat
# update-alternatives --install jstat.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstat.1.gz
update-alternatives --install /usr/bin/jstatd jstatd ${FakePath}/bin/jstatd ${Priority} --slave /usr/share/man/man1/jstatd.1 jstatd.1 ${FakePath}/man/man1/jstatd.1
update-alternatives --auto jstatd
# update-alternatives --install jstatd.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/jstatd.1.gz
update-alternatives --install /usr/bin/keytool keytool ${FakePath}/bin/keytool ${Priority} --slave /usr/share/man/man1/keytool.1 keytool.1 ${FakePath}/man/man1/keytool.1
update-alternatives --auto keytool
# update-alternatives --install keytool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/keytool.1.gz
update-alternatives --install /usr/lib/midbrowser/plugins/libjavaplugin.so midbrowser-javaplugin.so ${FakePath}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto midbrowser-javaplugin.so
update-alternatives --install /usr/lib/mozilla/plugins/libjavaplugin.so mozilla-javaplugin.so ${FakePath}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto mozilla-javaplugin.so
update-alternatives --install /usr/bin/native2ascii native2ascii ${FakePath}/bin/native2ascii ${Priority} --slave /usr/share/man/man1/native2ascii.1 native2ascii.1 ${FakePath}/man/man1/native2ascii.1
update-alternatives --auto native2ascii
# update-alternatives --install native2ascii.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/native2ascii.1.gz
update-alternatives --install /usr/bin/orbd orbd ${FakePath}/bin/orbd ${Priority} --slave /usr/share/man/man1/orbd.1 orbd.1 ${FakePath}/man/man1/orbd.1
update-alternatives --auto orbd
# update-alternatives --install orbd.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/orbd.1.gz
update-alternatives --install /usr/bin/pack200 pack200 ${FakePath}/bin/pack200 ${Priority} --slave /usr/share/man/man1/pack200.1 pack200.1 ${FakePath}/man/man1/pack200.1
update-alternatives --auto pack200
# update-alternatives --install pack200.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/pack200.1.gz
update-alternatives --install /usr/bin/policytool policytool ${FakePath}/bin/policytool ${Priority} --slave /usr/share/man/man1/policytool.1 policytool.1 ${FakePath}/man/man1/policytool.1
update-alternatives --auto policytool
#update-alternatives --install policytool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/policytool.1.gz
update-alternatives --install /usr/bin/rmic rmic ${FakePath}/bin/rmic ${Priority} --slave /usr/share/man/man1/rmic.1 rmic.1 ${FakePath}/man/man1/rmic.1
update-alternatives --auto rmic
# update-alternatives --install rmic.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/rmic.1.gz
update-alternatives --install /usr/bin/rmid rmid ${FakePath}/bin/rmid ${Priority} --slave /usr/share/man/man1/rmid.1 rmid.1  ${FakePath}/man/man1/rmid.1
update-alternatives --auto rmid
# update-alternatives --install rmid.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/rmid.1.gz
update-alternatives --install /usr/bin/rmiregistry rmiregistry ${FakePath}/bin/rmiregistry ${Priority} --slave /usr/share/man/man1/rmiregistry.1 rmiregistry.1 ${FakePath}/man/man1/rmiregistry.1
update-alternatives --auto rmiregistry
#update-alternatives --install rmiregistry.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/rmiregistry.1.gz
update-alternatives --install /usr/bin/schemagen schemagen ${FakePath}/bin/schemagen ${Priority} --slave /usr/share/man/man1/schemagen.1 schemagen.1 ${FakePath}/man/man1/schemagen.1
update-alternatives --auto schemagen
# update-alternatives --install schemagen.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/schemagen.1.gz
update-alternatives --install /usr/bin/serialver serialver ${FakePath}/bin/serialver ${Priority} --slave /usr/share/man/man1/serialver.1 serialver.1 ${FakePath}/man/man1/serialver.1
update-alternatives --auto serialver
# update-alternatives --install serialver.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/serialver.1.gz
update-alternatives --install /usr/bin/servertool servertool ${FakePath}/bin/servertool ${Priority} --slave /usr/share/man/man1/servertool.1 servertool.1 ${FakePath}/man/man1/servertool.1
update-alternatives --auto servertool
# update-alternatives --install servertool.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/servertool.1.gz
update-alternatives --install /usr/bin/tnameserv tnameserv ${FakePath}/bin/tnameserv ${Priority} --slave /usr/share/man/man1/tnameserv.1 tnameserv.1 ${FakePath}/man/man1/tnameserv.1
update-alternatives --auto tnameserv
# update-alternatives --install tnameserv.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/tnameserv.1.gz
update-alternatives --install /usr/bin/unpack200 unpack200 ${FakePath}/bin/unpack200 ${Priority} --slave /usr/share/man/man1/unpack200.1 unpack200.1 ${FakePath}/man/man1/unpack200.1
update-alternatives --auto unpack200
#update-alternatives --install unpack200.1.gz -> /usr/lib/jvm/java-6-sun/jre/man/man1/unpack200.1.gz
update-alternatives --install /usr/bin/wsgen wsgen ${FakePath}/bin/wsgen ${Priority} --slave  /usr/share/man/man1/wsgen.1 wsgen.1 ${FakePath}/man/man1/wsgen.1
update-alternatives --auto wsgen
# update-alternatives --install wsgen.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/wsgen.1.gz
update-alternatives --install /usr/bin/wsimport wsimport ${FakePath}/bin/wsimport ${Priority} --slave /usr/share/man/man1/wsimport.1 wsimport.1 ${FakePath}/man/man1/wsimport.1
update-alternatives --auto wsimport
# update-alternatives --install wsimport.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/wsimport.1.gz
update-alternatives --install /usr/bin/xjc xjc ${FakePath}/bin/xjc ${Priority} --slave /usr/share/man/man1/xjc.1 xjc.1 ${FakePath}/man/man1/xjc.1
update-alternatives --auto xjc
# update-alternatives --install xjc.1.gz -> /usr/lib/jvm/java-6-sun/man/man1/xjc.1.gz
update-alternatives --install /usr/lib/xulrunner-addons/plugins/libjavaplugin.so xulrunner-1.9-javaplugin.so ${FakePath}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto xulrunner-1.9-javaplugin.so
update-alternatives --install /usr/lib/xulrunner/plugins/libjavaplugin.so xulrunner-javaplugin.so ${FakePath}/jre/lib/${ARCH_FOLDER}/libnpjp2.so ${Priority}
update-alternatives --auto xulrunner-javaplugin.so



#EOF

