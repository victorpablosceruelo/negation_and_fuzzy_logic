:- module(loaded_classes,[get_class/1,add_class/1,set_main_class/1,main_class/1,
%	                  get_bytecode/1,add_bytecode/1,	      
	                  cleanup_loaded_classes/0,print/0],[assertions]).

:- doc(title,"Loaded classes for JVM Interpreter").

:- doc(author, "M. Zamalloa").

:- doc(module,"This module provides a suitable interface for a data structure 
	in which all loaded classes will be stored. It will play the role of the 
	old program term. Classes and interfaces will be represented resp. as 
	'class/8' and 'interface/7' facts. User classes are asserted in this module 
	by means of the 'add_class/1' predicate. Java pre-parsed libraries are stored 
	in the corresponding modules and will be accesible as if they were user 
	classes using the 'get_class/1' predicate.
	All the necessary information (class hierarchy, fields and methods, etc)
	could be obtained using JVML operations as usual.").

:- use_module(library(filenames), [no_path_file_name/2]).
:- use_module(library(write), [write/1]).

% JAVA LIBRARIES (Pre-parsed class files)
:- use_module(library(jvm_in_ciao(java_libraries('pre-parsed'(java(lang))))),
	[class/8, interface/7]).
:- use_module(library(jvm_in_ciao(java_libraries('pre-parsed'(java(util))))),
	[class/8, interface/7]).
:- use_module(library(jvm_in_ciao(java_libraries('pre-parsed'(java(io))))),
	[class/8, interface/7]).
:- use_module(library(jvm_in_ciao(java_libraries('pre-parsed'(java(lang(reflect)))))),
	[class/8,interface/7]).

:- data class/8.

class(A,B,C,D,E,F,G,H) :- lang:class(A,B,C,D,E,F,G,H).
class(A,B,C,D,E,F,G,H) :- util:class(A,B,C,D,E,F,G,H).
class(A,B,C,D,E,F,G,H) :- io:class(A,B,C,D,E,F,G,H).
class(A,B,C,D,E,F,G,H) :- reflect:class(A,B,C,D,E,F,G,H).

:- data interface/7.

interface(A,B,C,D,E,F,G) :- lang:interface(A,B,C,D,E,F,G).
interface(A,B,C,D,E,F,G) :- util:interface(A,B,C,D,E,F,G).
interface(A,B,C,D,E,F,G) :- io:interface(A,B,C,D,E,F,G).
interface(A,B,C,D,E,F,G) :- reflect:interface(A,B,C,D,E,F,G).

:- data main_class/1.
%:- data bytecode/5.

:- pred cleanup_loaded_classes #"It retracts all loaded classes and interfaces".
cleanup_loaded_classes :- 
	retractall_fact(loaded_classes:class(_,_,_,_,_,_,_,_)),
	retractall_fact(loaded_classes:interface(_,_,_,_,_,_,_)).
%	retractall_fact(main_class(_)).
%	retractall_fact(loaded_classes:bytecode(_,_,_,_,_)).

:- pred get_class/1 #"It reads a (user or library) class or interface from the database".
get_class(Class) :- 
%	Class = class(className(packageName(_PN),_),_,_,_,_,_,_,_),!,
	current_fact(loaded_classes:Class),!.
get_class(Class) :-
	Class = class(className(packageName(PN),_SCN),_,_,_,_,_,_,_),!,
	PN \== '',
	atom_concat(PNWB,'/',PN),
	no_path_file_name(PNWB,ModuleName),
	ModuleName:Class.
get_class(Interface) :- 
%	Interface = interface(interfaceName(packageName(''),_),_,_,_,_,_,_),!,
	current_fact(loaded_classes:Interface),!.
get_class(Interface) :-
	Interface = interface(interfaceName(packageName(PN),_SCN),_,_,_,_,_,_),!,
	PN \== '',
	atom_concat(PNWB,'/',PN),
	no_path_file_name(PNWB,ModuleName),
	ModuleName:Interface.


:- pred add_class/1 #"It adds a user class or interface to the database".
add_class(class(A,B,C,D,E,F,G,H)) :- 
	assertz_fact(loaded_classes:class(A,B,C,D,E,F,G,H)).
add_class(interface(A,B,C,D,E,F,G)) :- 
	assertz_fact(loaded_classes:interface(A,B,C,D,E,F,G)).

:- pred set_main_class/1 #"It sets the main class".
set_main_class(MC) :- set_fact(main_class(MC)).

%:- pred get_bytecode #"It reads a (user or library) bytecode from the database".
%get_bytecode(bytecode(A,B,C,D,E)) :- 
%	loaded_classes:bytecode(A,B,C,D,E),!.
%get_bytecode(BC) :-
%	BC = bytecode...,
%	atom_concat(PNWB,'/',PN),
%	no_path_file_name(PNWB,ModuleName),
%	ModuleName:Bytecode.

%:- pred add_bytecode #"It adds a bytecode instruction corresponding to a user class".
%add_bytecode(BC) :- assertz_fact(BC).

:- pred print #"Prints useful info. It could be adapted for any particular need".
print :- print_classes.%,nl,print_bytecodes.
print_classes :-
	loaded_classes:class(A,B,C,D,E,F,G,H),
	write(class(A,B,C,D,E,F,G,H)),nl,
	fail.
print_classes :-
	loaded_classes:interface(A,B,C,D,E,F,G),
	write(interface(A,B,C,D,E,F,G)),nl,
	fail.
print_classes.
%print_bytecodes :-
%	loaded_classes:bytecode(A,B,C,D,E),
%	write(bytecode(A,B,C,D,E)),nl,
%	fail.
%print_bytecodes.
