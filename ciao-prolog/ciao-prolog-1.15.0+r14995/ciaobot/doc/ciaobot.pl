:- module(ciaobot, _, [dcg, assertions, regtypes, basicmodes, fsyntax]).
%:- module(_,[main/0,main/1],[make,fsyntax,assertions]).

:- doc(filetype, application).

:- doc(title, "The Ciao Bot").
:- doc(subtitle, "An automated, distributed, compilation, and testing system for Ciao.").

:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").

%:- doc(logo, 'lpdoc-logo-128').

:- doc(summary, "This manual describes the @apl{Ciao Bot}, an
automated, distributed, and testing system. This system was primaraly
designed to test and generate packaged bundles for different platforms
and architectures, both in source and precompiled binary forms.").

:- doc(module, "This manual provides an overview of the main concepts
to understand, configure, and work with the @apl{Ciao Bot}. Note that
this system, although usable, is still in an early development stage.

@section{Overview of the System}

The distributed system distinguishes several machines, that we will
denote as:

@begin{itemize}
@item @bf{repository machine:} the machine where the source (SVN)
  repository for Ciao resides (the @tt{clip} server).

@item @bf{slave machines:} 0 or more physical or virtual machines that
  do the actual compilation, testing, packaging, etc.

@item @bf{master machine:} the machine that distributes work to the
  slave machines. It is in charge of (periodically) checking out the
  source from the repository, distributing it to the @em{slave
  machines}, and sending the messages that initiate the build jobs.
@end{itemize}

By default the @em{master} machine will poll changes in the Ciao SVN
every 15 minutes (see the file @tt{crontab.tmpl}).  If a change is
detected, then an initial building/testing process will start. Once
the initial compilation has finished, the system creates a source code
package, a @tt{tar} (@tt{.tgz}) file.  This file is placed in a common
directory, and the test process is started in the slave machines.
Both in the master and slave machines, part of the build test consists
of running the tests that have been placed in all the system, and
showing the errors that could appear to ensure that the system
behavior has not changed.

All the compilation and testing logs are saved in the directory
@tt{$$(BUILDROOTDIR)/log}. After each compilation, send the
differences between the new and previous logs to authors of each
commit (and to the @apl{Ciao Bot} maintainers). This facilitates the
task of running regression tests, looking at the exact point where an
error began to appear.

@section{Software Requirements}

The @apl{Compiler Farm} currently has the following software
requirements:

 @begin{itemize}

 @item The master machine must run a Linux-based operating system.

 @item Slave machines are required to test the source @tt{tar} file in
       other architectures and/or operating systems.  Note that a
       slave can act as a master machine of other slaves, this
       behavior allows us to reach private LANs or virtual machines
       running in the slave that are not visible to the master
       machine.

 @item Each master/slave machine builds and installs Ciao, runs the
       regression tests, and builds precompiled binary packages for
       its corresponding operating system. Thus, machines should have
       the SSH client and server configured, and should have all the
       software packages that Ciao requires installed (please read the
       @tt{README} file at the Ciao root directory). Additionally,
       other software requirements are:

     @begin{itemize}

     @item The \"@tt{sharutils}\" package, that contains the uuencode
     command.

     @item The \"@tt{expect}\" package, used to implement the
     timeout.sh script, but only in those platforms where no a timeout
     command is available.

     @item In a Windows guest, you have to add @tt{C:\Cygwin\bin} to
           the @tt{PATH} environment variable (Control Panel/System).
     @item @tt{alien} (in Debian machines, to generate @tt{.deb} from @tt{.rpm})
     @item Optionally, @href{http://www.virtualbox.org/}{VirtualBox}
           for virtual machines.
     @end{itemize}

 @end{itemize}

@section{Setting up Machines in the Compile Farm}
	
 @begin{itemize}

 @item Be sure that the accounts have the public keys configured
       correctly so that the process can be automated without having
       to enter passwords. In other words, you need to do the
       following:

       @begin{itemize} 

       @item The master machine must have an accessible, public IP
             address. The slave machines (0 or more), not necessary
             require a public IP adress.

       @item Be sure that the host names are correctly configured
             (file @tt{/etc/hosts}).  Note: in Windows it may be
             necessary to stop the DNS Client service.  To check the
             connections, you can use the @tt{'make -s testnet'}
             command

       @item Add the public key of the master and slave machines to the
             repository machine.

       @item Add the public keys of the slave machines to the master
             machine and vice versa.

       @item Add the public key of your account to the master machine.

       @item Add the public key of your account to the slave machines.
             This step is not required if you will use only the
             bypassrun make option to execute commands.

       @item Run at least once manually to ensure that in each host
             all the hosts are in the known hosts list.

       @end{itemize}

       For more information about how to use private and public keys
       for SSH authentication, read the SSH manual.

 @item Configure a limited user account in each machine and add a
       @tt{USERHOST[hostname]=username} in the @tt{SETTINGS} file for
       each machine (the master, the slaves, and the repository
       machine).  These accounts must not be in use by anybody,
       because they are used to isolate the activity of the tester,
       and to avoid unnecessary risks.

 @item Edit the file @tt{SETTINGS} and read the meaning of each variable
       that needs to be configured.  Note that the variable
       @tt{MASTERHOST} contains the name of the master machine.  The
       configuration file of each machine is in the file named
       @tt{$(HOSTNAME).cfg}, where @tt{$(HOSTNAME)} is the name of such machine.

@item You can easily add a new machine by copying one of the @tt{.cfg}
       files and configuring it for such new machine.  Then you can
       specify the machine that will master this new slave in the @tt{.cfg}
       file of the master, by adding it to the @tt{BUILDSLAVES_SRC},
       @tt{BUILDSLAVES_NOA} or @tt{BUILDSLAVES_BIN} variable.

 @item Do @tt{./ciaobot install}.  This will copy the required files to the
       master and slaves.

 @item (optional) Installing @tt{apache2-ciaotester-site} as
       @tt{/etc/apache2/sites-enabled/000-ciaotester}. This enables
       HTTP access to logs and packages from the master machine.

 @item In the master machine, restart the @tt{crond} daemon.

 @item In all machines, execute @tt{./ciaobot showinfo} and check that the
       variables are ok.

 @end{itemize}

@section{Frequently Asked Questions (FAQ)}

@begin{itemize}

@item @bf{Q:} How do I invoke the @apl{Compile Farm}?
@item @bf{A:} Most of the commands can be executed with:

@begin{verbatim}
$ cd ciaobot
$ ./ciaobot COMMAND
@end{verbatim}

@item @bf{Q:} If the @apl{Compile Farm} is working, will my recently
  committed version be built immediately?
@item @bf{A:} No. It will wait until the current work is finished in
  all machines. The @em{master} machine will not send any new
  compilation jobs unless all machines are idle.
@begin{alert}
Is that true?
@end{alert}


@item @bf{Q:} How often does the @apl{Compile Farm} build the last
  Ciao version?
@item @bf{A:} Each 5-15 minutes the @em{master machine} will check if
  a new version is available. If positive, and if no other build
  process is running, it will begin.


@item @bf{Q:} How can I force the build process without waiting?
@item @bf{A:} See the command in @file{cronfile.tmpl}, execute it from
  the master machine.
@begin{alert}
We need a command for that operation.
@end{alert}


@item @bf{Q:} How do I start the build process manually?
@item @bf{A:} There are a few basic options to start the build process manually:

 @begin{itemize}

  @item @tt{./ciaobot buildall}.  Start the build process in all machines.

  @item @tt{./ciaobot buildmaster}.  Start the build process only in the
    master machine.

  @item @tt{./ciaobot buildslaves}.  Start the build process in all the
    slave machines.

 @end{itemize}

@item @bf{Q:} How do I stop the build process manually?
@item @bf{A:} Use the @tt{kill} command.


@item @bf{Q:} How can I update the website manually? (to publish new packages)
@item @bf{A:} From the @tt{clip} server, execute:

@begin{verbatim}
$ su clip
$ cd ~/Systems/CiaoDE/trunk/website
$ ./create New
@end{verbatim}

@begin{alert}
The website is currently installed daily. It should happen more often
(even if LPdoc has not been optimized yet, the website installation
runs really fast).
@end{alert}

  The command will fetch new packages automatically from the master
  machine (@tt{cliptest1}).


@item @bf{Q:} The latest released version seems to be frozen.
@item @bf{A:} This may happen because some slave machine was not able
  to build its packaged bundle. Check the status of the machines and
  network (see below) to try to find out the problem.


@item @bf{Q:} How can I check the status of the machines and
  compilation?
@item @bf{A:} The status of the compilation is shown by the commands:

@begin{verbatim}
$ cd ciaobot
$ ./ciaobot status
@end{verbatim}


@item @bf{Q:} How do I check the network status?
@item @bf{A:} Execute the following commands:

@begin{verbatim}
$ cd ciaobot
$ ./ciaobot testnet
@end{verbatim}

    
@item @bf{Q:} How do I restart a virtual machine in the compilation farm?
@item @bf{A:} Execute the following commands from the master machine
  (@tt{cliptest1}):

@begin{verbatim}
$ /etc/init.d/virtualbox-HOSTNAME restart
@end{verbatim}

   where @tt{HOSTNAME} is one of the available machines.

@begin{alert}
Those files are not automatically installed. They should be.
@end{alert}

@item @bf{Q:} Why the Debian machine often fails to generate its
  packaged bundles?
@item @bf{A:} There is not a direct @tt{.deb} package generation for
  Ciao yet. Currently, it relies on @tt{alien} to generate a @tt{.deb}
  from a @tt{.rpm}. The Fedora machine generates its package and then
  it sends the package to the Debain machine. A failure in this
  sequence requires manual recompilation (or waiting for a new
  commit).


@end{itemize}
").

:- doc(bug, "Automate management of virtual machines (e.g. ports,
   etc. see @file{virtualbox.sh})").

:- doc(bug, "Install @tt{/etc/init.d/virtualbox-HOSTNAME} machines
   automatically. Those files were created by hand, which means that
   if @tt{cliptest1} breaks we would be in trouble.").

:- doc(bug, "Debian uses @tt{alien} to generate its packages from RPM. Do it directly").

:- doc(bug, "Slave machines test the source system and generate
   packages, but those packages are not installed (right?).").

:- doc(bug, "Reimplement in Prolog").

:- doc(bug, "Generalize the system to fetch the code from other
   sources, or testing other revisions").

:- doc(bug, "Add a version number in Ciaobot (or the build system)?
   E.g., a 'requires(ciaobot>=Ver)' entry in Manifest.pl so that we
   never give incorrect error messages in Ciaobot (this often happens
   when the system changes but we forget to update the code in the
   hosts). Another solution would be to update Ciaobot each time that
   a commit is done (but this does not work well for possible external
   users).").
