This project is now end-of-life, but it may prove necessary to rebuild parts of the web-site again.

This is one way to do so.

The project uses Maven 1; it looks as though the site dated February 2008 was built with version 1.1

This can be downloaded from:

http://archive.apache.org/dist/maven/binaries/
maven-1.1.zip|tar.gz

Maven 1.1 does appear to run OK under Java 1.6, but would originally have been run with 1.3.1 or 1.4.2

To rebuild the site files:

maven -Dmaven.test.skip=true site

It's important to skip the tests, as they seem to hang.