:- module(system, [], [pure, assertions, isomodes, regtypes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_compare)).
:- use_module(engine(term_typing)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(exceptions)).
:- use_module(engine(streams_basic)).
:- use_module(engine(arithmetic)).
:- use_module(engine(prolog_flags)).

:- doc(title, "Operating system utilities").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").

:- doc(module, "This module contains predicates for invoking
   services which are typically provided by the operating system.
   Note that the predicates which take names of files or directories
   as arguments in this module expect atoms, not @concept{path
   alias}es. I.e., generally these predicates will not call
   @pred{absolute_file_name/2} on names of files or directories taken
   as arguments.").

:- '$native_include_c_source'(.(system)). % TODO: rename to engine__system?

:- multifile define_flag/3.

%% This flag determines which path style is returned by the system
%% predicates which can return paths.  'os_dependent' means paths 
%% will be returned in the forma0-t the OS accepts it.  'posix' means
%% using a POSIX-style, with slashes, no drive letters or UNC paths,
%% and semicolons as separators.

define_flag(path_style, [posix, os_dependent], os_dependent).

% ---------------------------------------------------------------------------
:- export(pause/1).
:- doc(pause(Seconds), "Make this thread sleep for some @var{Seconds}.").
:- true pred pause(+int).
:- '$props'(pause/1, [impnat=cbool(prolog_pause)]).

% ---------------------------------------------------------------------------
:- export(time/1).
:- doc(time(Time), "@var{Time} is unified with the number of seconds
     elapsed since January, 1, 1970 (UTC).").
:- true pred time(?int).
:- '$props'(time/1, [impnat=cbool(prolog_time)]).

% ---------------------------------------------------------------------------
 %% :- doc(walltime(Time),"@var{Time} is unified with the time in
 %%      milliseconds elapsed in the real world since the last call to
 %%      @pred{walltime/1}. The first call returns a meaningless number.").
 %% 
 %% :- true pred walltime(?int).

% ---------------------------------------------------------------------------
:- export(datime/1).
:- doc(datime(Datime), "@var{Datime} is unified with a term of the
     form @tt{datime(Year,Month,Day,Hour,Minute,Second)} which contains
     the current date and time.").

:- true pred datime(?datime_struct).

datime(datime(Year,Month,Day,Hour,Min,Sec)) :-
        datime(_, Year, Month, Day, Hour, Min, Sec, _, _).

% ---------------------------------------------------------------------------
:- export(datime_struct/1).
:- true regtype datime_struct/1.

datime_struct(datime(Year,Month,Day,Hour,Min,Sec)) :-
        int(Year), int(Month), int(Day), int(Hour), int(Min), int(Sec).

% ---------------------------------------------------------------------------
:- export(datime/9).
:- doc(datime(Time,Year,Month,Day,Hour,Min,Sec,WeekDay,YearDay),
	"@var{Time} is as in @pred{time/1}. @var{WeekDay} is the number
	of days since Sunday, in the range 0 to 6.  @var{YearDay} is the
	number of days since January 1, in the range 0 to 365.").

:- true pred datime(+int,?int,?int,?int,?int,?int,?int,?int,?int)
        # "If @var{Time} is given, the rest of the arguments are unified
        with the date and time to which the @var{Time} argument refers.".

:- true pred datime(-int,?int,?int,?int,?int,?int,?int,?int,?int)
	# "Bound @var{Time} to current time and the rest of the
	arguments refer to current time.".

:- '$props'(datime/9, [impnat=cbool(prolog_datime)]).

% ---------------------------------------------------------------------------
:- export(copy_file/2).
:- doc(copy_file(Source,Destination), "Copies the file @{Source} to
	@{Destination}.").
:- true pred copy_file(+atm, +atm).

copy_file(Source, _Destination) :- \+ atom(Source), !,
	throw(error(domain_error(atom,Source),copy_file/2-1)).
copy_file(_Source, Destination) :- \+ atom(Destination), !,
	throw(error(domain_error(atom,Destination),copy_file/2-2)).
copy_file(Source, Destination) :-
	copy_file__2(Source, Destination).

:- '$props'(copy_file__2/2, [impnat=cbool(prolog_c_copy_file)]).

% ---------------------------------------------------------------------------
:- export(getenvstr/2).
:- doc(getenvstr(Name, Value), "The environment variable @var{Name}
    has @var{Value}.  Fails if variable @var{Name} is not defined.").

:- true pred getenvstr(+atm, ?string).

getenvstr(Name, _Value) :- \+ atom(Name), !,
	throw(error(domain_error(atom,Name),getenvstr/2-1)).
getenvstr(Name, Value) :-
	c_get_env(Name, Value2),
	atom_codes(Value2, Value).

:- '$props'(c_get_env/2, [impnat=cbool(prolog_c_get_env)]).

% ---------------------------------------------------------------------------
:- export(setenvstr/2).
:- doc(setenvstr(Name, Value), "The environment variable @var{Name}
    is assigned @var{Value}.").

:- true pred setenvstr(+atm, +string).

setenvstr(Name, _Value) :- \+ atom(Name), !,
	throw(error(domain_error(atom,Name),setenvstr/2-1)).
setenvstr(_Name, Value) :- \+ ( Value = [_|_] ; Value = [] ), !,
	throw(error(domain_error(character_code_list,Value),setenvstr/2-2)).
setenvstr(Name, Value) :-
	atom_codes(Value2, Value),
	c_set_env(Name, Value2).

% ---------------------------------------------------------------------------
:- export(set_env/2).
:- true pred set_env(+atm, +atm).
:- doc(set_env(Name, Value), "The environment variable @var{Name}
    is assigned @var{Value}.").

set_env(Name, _Value) :- \+ atom(Name), !,
	throw(error(domain_error(atom,Name),set_env/2-1)).
set_env(_Name, Value) :- \+ atom(Value), !,
	throw(error(domain_error(atom,Value),set_env/2-2)).
set_env(Name, Value) :-
	c_set_env(Name, Value).

:- '$props'(c_set_env/2, [impnat=cbool(prolog_c_set_env)]).

% ---------------------------------------------------------------------------
:- export(del_env/1).
:- true pred del_env(+atm).
:- doc(del_env(Name), "The environment variable @var{Name} is
   removed.").

del_env(Name) :- \+ atom(Name), !,
	throw(error(domain_error(atom,Name),del_env/1-1)).
del_env(Name) :-
	c_del_env(Name).

:- '$props'(c_del_env/1, [impnat=cbool(prolog_c_del_env)]).

% ---------------------------------------------------------------------------
:- export(current_env/2).
:- true pred current_env(?atm, ?atm).

:- doc(current_env(Name,Value), "If @var{Name} is an atom, then
   unifies the environment variable @var{Name} with its value. Note
   that this predicate can be used to enumerate all the environment
   variables using backtracking.").

current_env(Name, _Value) :- \+ ( var(Name); atom(Name) ), !,
	throw(error(domain_error(var_or_atom,Name),current_env/2-1)).
current_env(_Name, Value) :- \+ ( var(Value); atom(Value) ), !,
	throw(error(domain_error(var_or_atom,Value),current_env/2-2)).
current_env(Name, Value) :-
	( atom(Name) ->
	    c_get_env(Name,Value)
	; current_env__2(0, Name, Value)
	).

current_env__2(I, Name, Value) :-
	c_current_env(I, Name2, Value2),
	( Name=Name2, Value=Value2
	; J is I + 1,
	  current_env__2(J, Name, Value)
	).

:- '$props'(c_current_env/3, [impnat=cbool(prolog_c_current_env)]).

% ---------------------------------------------------------------------------
:- export(extract_paths/2).
:- doc(extract_paths(String, Paths), "Interpret @var{String} as the
   value of a UNIX environment variable holding a list of paths and
   return in @var{Paths} the list of the paths.  Paths in @var{String}
   are separated by colons, and an empty path is considered a shorthand
   for '.' (current path).  The most typical environment variable with this
   format is PATH.  For example, this is a typical use:
@begin{verbatim}
?- set_prolog_flag(write_strings, on).

yes
?- getenvstr('PATH', PATH), extract_paths(PATH, Paths).

PATH = "":/home/bardo/bin:/home/clip/bin:/opt/bin/:/bin"",
Paths = [""."",""/home/bardo/bin"",""/home/clip/bin"",""/opt/bin/"",""/bin""] ?

yes
?- 
@end{verbatim}
").

:- true pred extract_paths(+string, ?list(string)).

extract_paths([], ["."]).
extract_paths([C|Cs], [Path|Paths]) :-
        extract_path(C, Cs, ".", Path, Cs_),
        extract_paths_(Cs_, Paths).

extract_paths_([], []).
extract_paths_([_|Cs], Paths) :- % skip ":"
        extract_paths(Cs, Paths).

extract_path(0':, Cs, Path, Path, [0':|Cs]) :- !.
extract_path(C, [], _, [C], []) :- !.
extract_path(C, [D|Cs], _, [C|Path], Cs_) :-
        extract_path(D, Cs, [], Path, Cs_).

% ---------------------------------------------------------------------------
:- export(get_pid/1).
:- doc(get_pid(Pid), "Unifies @var{Pid} with the process
     identificator of the current process or thread.").
:- true pred get_pid(?int).
:- '$props'(get_pid/1, [impnat=cbool(prolog_getpid)]).

% ---------------------------------------------------------------------------
:- export(current_host/1).
:- doc(current_host(Hostname), "@var{Hostname} is unified with the
        fully qualified name of the host.").
:- true pred current_host(?atm).
:- '$props'(current_host/1, [impnat=cbool(prolog_current_host)]).

% ---------------------------------------------------------------------------
:- export(current_executable/1).
:- doc(current_executable(Path), "Unifies @var{Path} with the path
        to the current executable.").
:- true pred current_executable(?atm).
:- '$props'(current_executable/1, [impnat=cbool(prolog_current_executable)]).

% ---------------------------------------------------------------------------
:- export(umask/2).
:- doc(umask(OldMask, NewMask), "The process file creation mask was
    @var{OldMask}, and it is changed to @var{NewMask}.").

:- true pred umask(?int, +int).

:- true pred umask(OldMask, NewMask)
        : (var(OldMask), var(NewMask), OldMask == NewMask)
       => (int(OldMask), int(NewMask))
        # "Gets the process file creation mask without changing it.".

:- '$props'(umask/2, [impnat=cbool(prolog_unix_umask)]).

% ---------------------------------------------------------------------------
:- export(working_directory/2).
:- doc(working_directory(OldDir, NewDir),"Unifies current working
     directory with @var{OldDir}, and then changes the working
     directory to @var{NewDir}. Calling
     @tt{working_directory(Dir,Dir)} simply unifies @tt{Dir} with the
     current working directory without changing anything else.").

:- true pred working_directory(?atm, +atm).

:- true pred working_directory(OldDir, NewDir)
        : (var(OldDir), var(NewDir), OldDir == NewDir) => atm * atm
        # "Gets current working directory.".

:- '$props'(working_directory/2, [impnat=cbool(prolog_unix_cd)]).

% ---------------------------------------------------------------------------
:- export(cd/1).
:- doc(cd(Path), "Changes working directory to @var{Path}.").

:- true pred cd(+atm).

cd(Dir) :- working_directory(_, Dir).

% ---------------------------------------------------------------------------
:- export(shell/0).
:- true pred shell # "Execs the shell specified by the environment
   variable @tt{SHELL}. When the shell process terminates, control is
   returned to Prolog.".

:- '$props'(shell/0, [impnat=cbool(prolog_unix_shell0)]).

% ---------------------------------------------------------------------------
:- export(shell/1).
:- doc(shell(Command), "@var{Command} is executed in the shell
    specified by the environment variable @tt{SHELL}. It succeeds if
    the exit code is zero and fails otherwise.").

%%      On MSDOS or Windows, if `SHELL' is defined it is expected to
%%      name a UNIX like shell which will be invoked with the argument `-c
%%      COMMAND'. If `SHELL' is undefined, the shell named by `COMSPEC'
%%      will be invoked with the argument `/C COMMAND'.

:- true pred shell(+atm).

shell(Path) :- shell(Path, 0).

% ---------------------------------------------------------------------------
:- export(shell/2).
:- doc(shell(Command, ReturnCode), "Executes @var{Command} in the
     shell specified by the environment variable @tt{SHELL} and stores
     the exit code in @var{ReturnCode}.").

:- true pred shell(+atm, ?int).

:- '$props'(shell/2, [impnat=cbool(prolog_unix_shell2)]).

% ---------------------------------------------------------------------------
:- export(system/1).
:- doc(system(Command), "Executes @var{Command} using the shell
        @apl{/bin/sh}.").

:- true pred system(+atm).

system(Path) :- system(Path, _Status).

% ---------------------------------------------------------------------------
:- export(system/2).
:- doc(system(Command, ReturnCode), "Executes @var{Command} in the
     @apl{/bin/sh} shell and stores the exit code in @var{ReturnCode}.").

:- true pred system(+atm, ?int).

:- '$props'(system/2, [impnat=cbool(prolog_unix_system2)]).

% ---------------------------------------------------------------------------
:- export(exec/4).
:- doc(exec(Command, StdIn, StdOut, StdErr), "Starts the process
@var{Command} and returns the standart I/O streams of the process in
@var{StdIn}, @var{StdOut}, and @var{StdErr}.").

:- true pred exec(+atm, -stream, -stream, -stream).

exec(Command, StdIn, StdOut, StdErr):- 
        '$exec'(Command, StdIn, StdOut, StdErr).

% ---------------------------------------------------------------------------
:- export(exec/3).
:- doc(exec(Command, StdIn, StdOut), "Starts the process
@var{Command} and returns the standart I/O streams of the process in
@var{StdIn} and @var{StdOut}. @tt{Standard error} is connected to
whichever the parent process had it connected to.").

:- true pred exec(+atm, -stream, -stream).

exec(Command, StdIn, StdOut):- 
        '$exec'(Command, StdIn, StdOut, []).

 %% :- doc(bug, "When reading from a exec'ed process, the 'end of
 %% file' condition (when the launched process finishes) somehow
 %% propagates to the standard input of the Ciao Prolog process, thus
 %% causing subsequent Ciao Prolog reads to return the 'end of file'
 %% condition.").
% ---------------------------------------------------------------------------
% Internal exec predicate
%:- export('$exec'/4).
:- '$props'('$exec'/4, [impnat=cbool(prolog_exec)]).

% ---------------------------------------------------------------------------
% Internal exec predicate
:- export('$find_file'/8). % used by streams_basic:absolute_file_name/?
:- '$props'('$find_file'/8, [impnat=cbool(prolog_find_file)]).

% ---------------------------------------------------------------------------
:- export(popen/3).
:- doc(popen(Command, Mode, Stream), "Open a pipe to process
    @var{Command} in a new shell with a given @var{Mode} and return a
    communication @var{Stream} (as in UNIX @tt{popen(3)}). If
    @var{Mode} is @tt{read} the output from the process is sent to
    @var{Stream}. If @var{Mode} is @tt{write}, @tt{Stream} is sent as
    input to the process. @var{Stream} may be read from or written
    into using the ordinary stream I/O predicates. @var{Stream} must
    be closed explicitly using @pred{close/1}, i.e., it is not closed
    automatically when the process dies.

").

:- true pred popen(+atm, +popen_mode, -stream).

popen(Command, Mode, S) :-
	nonvar(Mode),
	popen_mode(Mode),
	atom(Command), !,
	'$unix_popen'(Command, Mode, S0), !, S=S0.

% internal predicate
:- '$props'('$unix_popen'/3, [impnat=cbool(prolog_unix_popen)]).

% ---------------------------------------------------------------------------
:- export(popen_mode/1).
:- regtype popen_mode(M)
  # "@var{M} is 'read' or 'write'.".

popen_mode(read).
popen_mode(write).

% ---------------------------------------------------------------------------
:- export(directory_files/2).
:- doc(directory_files(Directory, FileList), "@var{FileList} is the
        unordered list of entries (files, directories, etc.) in
        @var{Directory}.").
:- true pred directory_files(+atm,?list(atm)).
:- '$props'(directory_files/2, [impnat=cbool(prolog_directory_files)]).

% ---------------------------------------------------------------------------
:- export(mktemp/2).
:- doc(mktemp(Template, Filename), "Returns a unique
   @var{Filename} based on @var{Template}: @var{Template} must be a
   valid file name with six trailing X, which are substituted to
   create a new file name.  @var{Filename} is created in read/write mode 
   but closed immediately after creation.").
:- trust pred mktemp(+atm, ?atm).
:- '$props'(mktemp/2, [impnat=cbool(prolog_unix_mktemp)]).

% ---------------------------------------------------------------------------
:- export(file_exists/1).
:- doc(file_exists(File), "Succeeds if @var{File} (a file or
        directory) exists (and is accessible).").

:- true pred file_exists(+atm).

file_exists(Path) :- file_exists(Path, 0).

% ---------------------------------------------------------------------------
:- export(file_exists/2).
:- doc(file_exists(File, Mode), "@var{File} (a file or directory)
        exists and it is accessible with @var{Mode}, as in the Unix
        call @tt{access(2)}. Typically, @var{Mode} is 4 for read
        permission, 2 for write permission and 1 for execute
        permission.").
:- true pred file_exists(+atm, +int).
:- '$props'(file_exists/2, [impnat=cbool(prolog_unix_access)]).

% ---------------------------------------------------------------------------
:- export(file_property/2).
:- doc(file_property(File, Property), "@var{File} has the property
   @var{Property}. The possible properties are:

@begin{description}

@item{type(@var{Type})} @var{Type} is one of @tt{regular}, @tt{directory},
      @tt{symlink}, @tt{fifo}, @tt{socket} or @tt{unknown}.

@item{linkto(@var{Linkto})} If @var{File} is a symbolic link,
      @var{Linkto} is the file pointed to by the link (and the other
      properties come from that file, not from the link itself).

@item{mod_time(@var{ModTime})} @var{ModTime} is the time of last
      modification (seconds since January, 1, 1970).

@item{mode(@var{Protection})} @var{Protection} is the protection mode.

@item{size(@var{Size})} @var{Size} is the size.

@end{description}

   If @var{Property} is uninstantiated, the predicate will enumerate the
   properties on backtracking.").

:- true pred file_property(+atm, ?struct).

file_property(Path, Property) :-
        file_property_(Property, Path).

file_property_(Property, Path) :-
        var(Property), !,
        file_properties(Path, Type, Linkto, Time, Protection, Size),
        ( Property = type(Type)
        ; Linkto \== '', Property = linkto(Linkto)
        ; Property = mod_time(Time)
        ; Property = mode(Protection)
        ; Property = size(Size)
        ).
file_property_(type(Type), Path) :- !,
        file_properties(Path, Type0, [], [], [], []),
        Type = Type0.
file_property_(linkto(File), Path) :- !,
        file_properties(Path, [], File0, [], [], []),
        File0 \== '',
        File = File0.
file_property_(mod_time(Time), Path) :- !,
        file_properties(Path, [], [], Time, [], []).
file_property_(mode(Protection), Path) :- !,
        file_properties(Path, [], [], [], Protection, []).
file_property_(size(Size), Path) :- !,
        file_properties(Path, [], [], [], [], Size).
file_property_(Other, _) :-
        throw(error(domain_error(file_property_type,Other),
                    file_property/2-2)).

% ---------------------------------------------------------------------------
:- export(file_properties/6).
:- doc(file_properties(Path, Type, Linkto, Time, Protection, Size),
        "The file @var{Path} has the following properties:

@begin{itemize} 

@item File type @var{Type} (one of @tt{regular}, @tt{directory},
      @tt{symlink}, @tt{fifo}, @tt{socket} or @tt{unknown}).

@item If @var{Path} is a symbolic link, @var{Linkto} is the file pointed
      to.  All other properties come from the file pointed, not the
      link.  @var{Linkto} is '' if @var{Path} is not a symbolic link.

@item Time of last modification @var{Time} (seconds since January, 1,
      1970).

@item Protection mode @var{Protection}.

@item Size in bytes @var{Size}.

@end{itemize}
").

:- true pred file_properties(+atm, ?atm, ?atm, ?int, ?int, ?int).
:- '$props'(file_properties/6, [impnat=cbool(prolog_file_properties)]).

% ---------------------------------------------------------------------------
:- export(modif_time/2).
:- doc(modif_time(File, Time), "The file @var{File} was last
     modified at @var{Time}, which is in seconds since January, 1,
     1970. Fails if @var{File} does not exist.").

:- true pred modif_time(+atm, ?int).

modif_time(Path, Time) :-
        prolog_flag(fileerrors, OldFE, off),
        ( file_properties(Path, [], [], Time, [], []) ->
            set_prolog_flag(fileerrors, OldFE)
        ; set_prolog_flag(fileerrors, OldFE),
          fail
        ).

% ---------------------------------------------------------------------------
:- export(modif_time0/2).
:- doc(modif_time0(File, Time), "If @var{File} exists, @var{Time} is
      its latest modification time, as in @pred{modif_time/2}.
      Otherwise, if @var{File} does not exist, @var{Time} is zero.").

:- true pred modif_time0(+atm, ?int).

modif_time0(Path, Time) :-
        prolog_flag(fileerrors, OldFE, off),
        ( file_properties(Path, [], [], T, [], []), !
        ; T = 0
        ),
        set_prolog_flag(fileerrors, OldFE),
        Time = T.

% ---------------------------------------------------------------------------
:- export(fmode/2).
:- doc(fmode(File, Mode), "The file @var{File} has protection mode
        @var{Mode}.").

:- true pred fmode(+atm, ?int).

fmode(Path, Mode) :-
        file_properties(Path, [], [], [], Mode, []).

% ---------------------------------------------------------------------------
:- export(chmod/2).
:- doc(chmod(File, NewMode), "Change the protection mode of file
        @var{File} to @var{NewMode}.").

:- true pred chmod(+atm, +int).
:- '$props'(chmod/2, [impnat=cbool(prolog_unix_chmod)]).

% ---------------------------------------------------------------------------
:- export(chmod/3).
:- doc(chmod(File, OldMode, NewMode), "The file @var{File} has
    protection mode @var{OldMode} and it is changed to @var{NewMode}.").

:- true pred chmod(+atm, ?int, +int).

:- true pred chmod(File, OldMode, NewMode)
        : (atm(File), var(OldMode), var(NewMode), OldMode == NewMode)
        => atm * atm * atm
        # "Equivalent to fmode(@var{File},@var{OldMode})".

chmod(Path, OldMode, NewMode) :-
        OldMode == NewMode, !,
        fmode(Path, OldMode).
chmod(Path, OldMode, NewMode) :-
        fmode(Path, OldMode),
        chmod(Path, NewMode).


% ---------------------------------------------------------------------------
:- export(delete_directory/1).
:- doc(delete_directory(File), "Delete the directory @var{Directory}.").
:- pred delete_directory(+atm).
:- '$props'(delete_directory/1, [impnat=cbool(prolog_unix_rmdir)]).

% ---------------------------------------------------------------------------
:- export(delete_file/1).
:- doc(delete_file(File), "Delete the file @var{File}.").
:- pred delete_file(+atm).
:- '$props'(delete_file/1, [impnat=cbool(prolog_unix_delete)]).

% ---------------------------------------------------------------------------
:- export(rename_file/2).
:- doc(rename_file(File1, File2), 
        "Change the name of  @var{File1} to @var{File2}.").
:- pred rename_file(+atm,+atm).
:- '$props'(rename_file/2, [impnat=cbool(prolog_unix_rename)]).

% ---------------------------------------------------------------------------
:- export(make_directory/2).
:- doc(make_directory(DirName, Mode), 
        "Creates the directory @var{DirName} with a given @var{Mode}.  This is, as usual, operated against the current umask value.").
:- pred make_directory(+atm, +int).
:- '$props'(make_directory/2, [impnat=cbool(prolog_unix_mkdir)]).

% ---------------------------------------------------------------------------
:- export(make_directory/1).
:- doc(make_directory(DirName),
        "Equivalent to @tt{make_directory(D,0o777)}.").

:- pred make_directory(+atm).

make_directory(D) :-
        make_directory(D,0o777).

% ---------------------------------------------------------------------------
:- export(make_dirpath/2).
:- pred make_dirpath(+atm, +int).

:- doc(make_dirpath(Path, Mode),
        "Creates the whole @var{Path} for a given directory with a given @var{Mode}. As an example, @tt{make_dirpath('/tmp/var/mydir/otherdir')}."). 

make_dirpath(Path, Mode) :-
        atom_codes(Path, PathCodes),
        (          % If relative, transform it into absolute
            PathCodes = "/"||_ ->
            AbsPathCodes = PathCodes
        ;
            working_directory(CurrentDir, CurrentDir),
            atom_codes(CurrentDir, CurrentDirCodes),
            append(CurrentDirCodes, "/"||PathCodes, AbsPathCodes)
        ),
        make_abs_dir(AbsPathCodes, '', Mode).

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

% 
% Making the intermediate directories: instead of cd'ing to
% directories (which is something which can break and modify the
% program state), we construct incrementally the intermediate directories.
% The idea here is to traverse the absolute path and construct 
% partial paths, which are appended to an atom which represents the
% (initially empty) path.  Depending on the implementation of
% atom_concat/3 (and of the atoms), this can be done quite fast if the
% first argument does not need to be completely traversed.

% End of path
make_abs_dir("", _, _Mode):- !.
% The recursive case: perform a step in the recursion and construct the
% intermediate directory.
make_abs_dir(Path, IncPath, Mode):-
        decompose(Path, RestPath, Component-[]),
        % Transform into atom and add to partial path
        atom_codes(PartComp, Component),
        atom_concat(IncPath, PartComp, NewPath),
        (
            file_exists(NewPath) ->
            true
        ;
            make_directory(NewPath, Mode)
        ),
        make_abs_dir(RestPath, NewPath, Mode).

% Collapse double slashes into only one (that is the Linux/Unix convention)
decompose("//"||PathWOSlash, RestPath, Queue):- !, 
        decompose("/"||PathWOSlash, RestPath, Queue).
decompose("/"||PathWOSlash, RestPath, "/"||Queue-TailQ):-
        decompose_aux(PathWOSlash, RestPath, Queue-TailQ).
decompose_aux("", "", Q-Q).
decompose_aux("/"||P, "/"||P, Q-Q):- !.
decompose_aux([P|Ps], RestP, [P|RestQ]-TailQ):- 
        decompose_aux(Ps, RestP, RestQ-TailQ).



 %% make_dirpath(Path, Mode) :-
 %% 	working_directory(CurrentDir, CurrentDir),
 %% 	make_dirpath_aux(Path, Mode),
 %% 	working_directory(_, CurrentDir).
 %% 
 %% make_dirpath_aux(Path, Mode) :-
 %% 	atom_concat(Head, Tail, Path),
 %% 	atom_concat('/', SubTail, Tail), !,
 %% 	(Head = '' ->
 %% 	 working_directory(CurrentDir, '/')
 %% 	;
 %% 	 ((file_exists(Head), file_property(Head, type(directory)))
 %% 	  ;
 %% 	   make_directory(Head, Mode) 
 %% 	 ),
 %% 	 working_directory(CurrentDir, Head)),
 %% 	make_dirpath_aux(SubTail, Mode).
 %% make_dirpath_aux(Dir, Mode) :-
 %% 	make_directory(Dir, Mode).
 %% 	 



% ---------------------------------------------------------------------------
:- export(make_dirpath/1).
:- pred make_dirpath(+atm).

:- doc(make_dirpath(Path),
                "Equivalent to @tt{make_dirpath(D,0o777)}.").
	  
make_dirpath(Path) :-
        make_dirpath(Path, 0o777) .

% ---------------------------------------------------------------------------
:- export(cyg2win/3).
:- doc(cyg2win(+CygWinPath, ?WindowsPath, +SpawSlash), "Converts a
path in the CygWin style to a Windows-style path, rewriting the driver
part.  If @var{SwapSlash} is @tt{swap}, slashes are converted in to
backslash.  If it is @tt{noswap}, they are preserved.").

cyg2win("/cygdrive/"||[D,0'/ | Dir], [D,0':| Path],Swap) :- !,
						             % New Drive notat.
        swapslash(Swap,[0'/ | Dir],Path).
cyg2win("//"||[D,0'/ | Dir], [D,0':,0'\\ | Path],Swap) :- !, % Drive letter
        swapslash(Swap,Dir,Path).
cyg2win("//"||Dir, "\\\\"||Path,Swap) :- !,                  % Network drive
        swapslash(Swap,Dir,Path).
cyg2win(Dir,  [D,0': |Path],Swap) :-                         % Default drive
        swapslash(Swap,Dir,Path),
        default_drive(D).

default_drive(D) :-
        getenvstr('COMSPEC',[D|_]), !. % Shell command interpreter' drive
default_drive(0'C).                    % If not defined, assume C

swapslash(noswap,Dir,Dir) :-
	!.
swapslash(swap,Dir,Path) :-
	do_swapslash(Dir,Path).

do_swapslash([],[]).
do_swapslash([0'/|D],[0'\\|ND]) :- !,
        do_swapslash(D,ND).
do_swapslash([C|D],[C|ND]) :-
        do_swapslash(D,ND).
