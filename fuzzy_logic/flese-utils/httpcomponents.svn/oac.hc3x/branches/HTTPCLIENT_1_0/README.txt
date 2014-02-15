Jakarta Commons HTTP Client
===========================

Welcome to the httpclient component of the Jakarta Commons
project.

BUILDING
--------

To compile the source, you'll want the excellent Ant utility.
It can be found here :

  http://jakarta.apache.org/ant/

You'll also need:
 * an implementation of the Java Secure Socket Extension,
   a standard Java extension available from:
     http://java.sun.com/products/jsse

 * the log4j logging component, available from:
     http://jakarta.apache.org/log4j

To help the build and test targets find these classes,
you must make a copy of the build.properties.sample file,
rename to build.properties, and modify it to reflect the
location of jsse.jar, jnet.jar, jcert.jar and log4j.jar
on your computer.

Once you have Ant, JSSE and log4j properly installed, and
your build.properties file properly configured, you are ready
to build the component:

To build a jar :

$ ant dist

To build the API documentation :

$ ant javadoc

To build the jar and API doc at once :

$ ant dist

Run ant -projecthelp to see a list of all available targets.


TESTING
-------

For testing the project, you will need JUnit:

  http://www.junit.org/

To let the test process find JUnit, you may make a
copy of the build.properties.sample file, rename to
build.properties,  and modify it to reflect
the location of the junit.jar on your computer.

(Note that JUnit is only needed to build and execute
the unit tests.  It is not needed to build the
"regular" classes, nor is it needed at runtime.)

To compile and test the component :

$ ant test


LOGGING
-------

HTTPClient uses log4j for logging.  You'll need a copy of
log4j.jar (available from http://jakarta.apache.org/log4j)
in order to compile and run httpclient.

To configure logging, add a file named "log4j.properties"
to your classpath. (The first one found will be used.)
There is an example log4j.properties file in the
/src/conf directory.


HTTPS SUPPORT
-------------

To support the HTTPS protocol, httpclient uses an
implementation of the Java Secure Socket Extension.

Note that JSSE is only needed at runtime if you want
to use the HTTPS protocol.  You will need it to
build from source, however.

You'll need to set up the JSSE within your Java
security configuration. You can do this by configuring
your JRE installation, or you may do this in code.

To configure JSSE within your JRE, edit the java.security file
(typically in [JAVA-HOME]/jre/lib/security) and add the line:

   security.provider.<n>=com.sun.net.ssl.internal.ssl.Provider

where <n> is one greater than the existing security.provider.*
properties.

NOTE: You may have multiple copies of the JRE installation.
If you're having trouble, you may not have modified the
right file. Search for "java.security" to find additional copies.

You will also need to set the Java system property:

   java.protocol.handler.pkgs

to include your JSSE provider. For Sun's implementation of
JSSE, this is:

   com.sun.net.ssl.internal.www.protocol

For example:

   -Djava.protocol.handler.pkgs=com.sun.net.ssl.internal.www.protocol


Alternatively, you may configure the JSSE support in your code, by
invoking the following lines:

      // add the provider
      Security.addProvider(new com.sun.net.ssl.internal.ssl.Provider());
      // set the property
      System.setProperty("java.protocol.handler.pkgs",
                         "com.sun.net.ssl.internal.www.protocol");
