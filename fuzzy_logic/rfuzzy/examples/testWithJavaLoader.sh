#!/bin/bash

if [ -z "$1" ] || [ "$1" == "" ]; then 
	CLASSPATH_JAVALL="/usr/share/CiaoDE/ciao/library/javall"
else
	CLASSPATH_JAVALL="$1"
fi

PWD=`pwd`

javac -classpath ${CLASSPATH_JAVALL} javaInterface_javaLoader.java
javac -classpath ${CLASSPATH_JAVALL} javaInterface_example0.java

java -cp ${CLASSPATH_JAVALL}:./ javaInterface_example0 ${CLASSPATH_JAVALL}/plserver 


# /home/vpablos/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/rfuzzy_ciao/examples/restaurant.pl
for file in *.pl ; do
	# moduleToLoad="${PWD}/${file}"
	moduleToLoad="${file}"
	echo " "
	echo " -> Trying with example file ${moduleToLoad} "
	echo " "
	java -cp ${CLASSPATH_JAVALL}:./ javaInterface_javaLoader ${CLASSPATH_JAVALL}/plserver ${moduleToLoad}
	echo " "
	read -p "enter to continue ..."
done
echo " "

