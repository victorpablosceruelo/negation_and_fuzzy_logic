%%
% Test to call primitive structures
:- module(test1, [test10/0,test11/0, test12/0, main/0], [assertions,nativeprops, basicmodes,unittestprops]).

:- use_module(library(pythonll(pythonsock))).
:- use_module(library(pythonll(pythonrt))).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(terms)).
:- use_module(library(strings)).
:- use_module(library(write)).
:- use_module(library(unittest)).

:- doc(module, "Python inside Prolog: unit tests").

:- dynamic python_started/1.

python_started('no').
main :-
	set_prolog_flag(write_strings, on),
	python_start,
	run_tests([(test10,'test10'),
			   (test11,'test11'),
			   (test12,'test12')]),
	pause(3600).

run_tests([]).
run_tests([(T,N)|R]) :- run_test(T, N), run_tests(R).
run_test(Test, Name) :-
	atom_concat(['run ', Name, ': '], Start), atom_concat([Name, ' finished'], End),
	display(Start), Test, display(End), nl.

%% NOTES:
%  Every interpretation that fails instanciates a prolog variable to a
%  prolog structure, with name $python_exception, and one param [string_error]
%% ----------------------------------------------------
%  Custom class instanciation
%% ----------------------------------------------------

check_test(Equality) :-
	(Equality ->
		display('\ttest succeeded')
		;
		display('\ttest failed')), nl.

% Success-Test module import and method invocation 0-param with return value
test10:- 
	% setfact(python_started('yes')), % so other tests do not run again?
	python_import_module('sys', SysModule),
	python_invoke_method(SysModule, getdefaultencoding(E)),
	check_test(E == "ascii").

% Failure-Test module import ('anything') does not exist
test11:- 
	% todo: check that import error exception was thrown.
	catch(python_import_module('anything', NoModule), _, check_test(false)).

% Failure-Test module call to unexisting method- AttributeError exception thrown
test12:-
	python_import_module('os', OsModule),
	catch(python_invoke_method(OsModule, getdefaultencoding(_)), Ex, true),
	functor(Ex, ExName, _), % todo: adapt python_exception structure to get error type.
	check_test(ExName == 'python_exception').

% unittest lib
%:- test test10(E) : display('coucou'), E = 'ascii' + not_fails # "Test OK".

