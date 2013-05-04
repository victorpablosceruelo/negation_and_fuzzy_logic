:- module(test_java, [main/0,set_classpath/1], [assertions]).

:- use_module(library(format)).
:- use_module(library(iso_byte_char)).
:- use_module(library(javall(javart))).
:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(.('../misc_commands')).

%% Before any test, current directory must be set to
%% this directory.

:- test main.

main:-
	%
	% Changing directory... 
 	working_directory(OldDir,OldDir),
 	name(OldDir,OldDirS),
 	append(OldDirS,"/test_java",NewDirS),
 	name(NewDir,NewDirS),
 	working_directory(OldDir,NewDir),
	%
	format("Compiling files~n",[]),
	compile_files,
	!,
        format("Testing Java-to-Prolog interface~n",[]),
	j2pl_test,
        format("Testing Prolog-to-Java interface~n",[]),
	pl2j_test,
	%
	% Restoring directory.
	working_directory(NewDir,OldDir),
	!.


% -------------------------------------------------------------------
% File compilation.
% -------------------------------------------------------------------

compile_files:-
	set_classpath("./"),
%jcf%	name(ClassPath, Cp2),
%jcf%	do_command(['javac -classpath ', ClassPath,' ','j2pl_test.java']),
	do_exec(['javac',' ','j2pl_test.java']),
	ciaoc_compile(plserver).

do_exec(CmdLst):-
	atom_concat(CmdLst,Cmd),
	format("executing command: ~a~n",[Cmd]),
	exec(Cmd,_,_,Err),
	print_errors(Err).

print_errors(S):-
	repeat,
	get_line(S,L),
	(L = end_of_file ->
	 true
	;
	 format("~s~n",[L]),
	 fail
	).
	

% -------------------------------------------------------------------
% The directory name madness.
% -------------------------------------------------------------------
set_classpath(Path):-
	compound_classpath(Path, Cp),
	setenvstr('CLASSPATH',Cp).
	
compound_classpath(UserClasspath,NewClasspath) :-
        absolute_file_name(library(javall(javart)),AbsFileName),
        name(AbsFileName,AbsFileNameS),
        append(UClasspath,"/javart.pl",AbsFileNameS),
	correct_win_path(UClasspath,CiaoClasspath,System),
	addPath(CiaoClasspath,UserClasspath,System,NewClasspath).

addPath(Cp,"",_,Cp).

addPath("",Cp,_,Cp).

addPath(Cp1,Cp2,windows,Cp) :-
	change_slashes(Cp1,Cp1s),
	change_slashes(Cp2,Cp2s),
%	append([0'"|Cp1s],[0'",0';,0'"|Cp2s],Cps),
%	append(Cps,[0'"],Cp).
	append(Cp1s,[0';|Cp2s],Cp).
%

addPath(Cp1,Cp2,other,Cp) :-
	append(Cp1,[0':|Cp2],Cp).

correct_win_path([0'/,0'c,0'y,0'g,0'd,0'r,0'i,0'v,0'e,0'/,L,0'/|Upath],[L,C,Bs|Wpath],windows):-
	!,
	char_code('\\',Bs),
	char_code(':',C),
	member(L,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
	change_slashes(Upath, Wpath).

correct_win_path([0'/,0'/,L,0'/|Upath], [L,0':,Bs|Wpath],windows):-
	char_code('\\',Bs),
	member(L,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
	change_slashes(Upath, Wpath).

correct_win_path(X,X,other).
	
change_slashes([],[]).

change_slashes([0'/|Upath],[0'\\|Wpath]):-
	!,
	change_slashes(Upath,Wpath).

change_slashes([L|Upath],[L|Wpath]):-
	change_slashes(Upath,Wpath).

% -------------------------------------------------------------------
% Java to Prolog test.
% -------------------------------------------------------------------

j2pl_test:-
	j2pl_command(Cmd),
	do_exec(Cmd),
	format("Java-to-Prolog test terminated~n",[]).

j2pl_test:-
	format("ERROR: Java-to-Prolog test failed~n",[]).

j2pl_command(['java',' ','j2pl_test',' ',PLServer]):-
%jcf%j2pl_command(['java -classpath',' ',ClassPath,' ','j2pl_test',' ',PLServer]):-
%jcf%	compound_classpath("./", Cp2),
%jcf%	name(ClassPath, Cp2),
	detect_OS(Os),
	(Os = windows ->
	 absolute_file_name(library(javall(javart)),Cmd1),
	 name(Cmd1,Cmd1s),
	 append(Cmd2s,"/library/javall/javart.pl",Cmd1s),
	 get_os(OS), atom_codes(OS, OSC),
	 get_arch(Arch), atom_codes(Arch, ArchC),
	 list_concat([Cmd2s,"/bin/", OSC, ArchC,
	     "/ciaoengine.exe %* -C -i -b ./plserver"],Cmd3s),
         correct_win_path(Cmd3s,Cmd4s,_),
	 name(PLServer,Cmd4s)
	;
	 PLServer = './plserver'
	).

detect_OS(Os):-
	absolute_file_name(library(javall(javart)),X),
	name(X,Xs),
	correct_win_path(Xs,_,Os).

% -------------------------------------------------------------------
% Prolog to Java test.
% -------------------------------------------------------------------

pl2j_test:-
	start_pl2j_test,
	format("Prolog-to-Java test terminated~n",[]).

pl2j_test:-
	format("ERROR: Prolog-to-Java test failed~n",[]).
	
start_pl2j_test:-
	java_start,
	%% Warning: this test probably does not run in a batch
        %% script. This should be tested.
	java_create_object('java.awt.Frame'('Prueba'),Frame),
	java_invoke_method(Frame,resize(300,300,_)),
	java_invoke_method(Frame,setLocation(1,1,_)),
	java_delete_object(Frame),
	java_create_object('java.lang.String'('prueba'),Str),
	java_invoke_method(Str,substring(1,4,SubStr)),
	SubStr = "rue",
	java_stop,
	format("Prolog-to-Java test succeeded~n",[]).
