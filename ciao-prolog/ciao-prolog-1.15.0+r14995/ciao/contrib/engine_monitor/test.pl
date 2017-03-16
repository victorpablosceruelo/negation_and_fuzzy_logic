:- module(test, [], []).

% Some intereting inputs:
%   ./build.sh
%   ./test
%   ['/Users/jfran/Documents/svn/ciao-n/optim_comp/testsuite/tests/boyer/boyer'].
%   main.

:- use_module(library(engine_monitor)).
:- use_module(library(system)).

% We need concurrency for the engine monitor
% (it is implemented using GLUT, which can only execute in the main thread)
:- use_module(library(concurrency)).

:- export(main/1).
main(Args) :-
	display(user_error, '[Engine monitor enabled]'), nl(user_error),
%	toplevel(Args),
	toplevel_thread(Args, Id),
	%
	display(threadid(Id)), nl,
	monitor_eng(Id),
	begin_monitor,
	% (because of GLUT, the program never reaches this point...)
	eng_wait(Id),
	eng_release(Id).

toplevel_thread(Args, Id) :-
	% Do not wait
%	eng_call(toplevel(Args), create, create, Id).
	eng_call(mytoplevel(Args), create, create, Id).

:- use_module(library(write)).
:- use_module(library(read)).

%:- use_module(library(toplevel)).
:- use_module(library(compiler)).

% A minimal toplevel
% TODO: the good one does not work, why? maybe because it reads from tty
mytoplevel(_) :-
	( repeat,
	  read(X),
	  ( X = halt ->
	      !
%	  ; display('Not recognized '), write(X), nl,
%	    loop
	  ; execute(X),
	    loop
	  )
	; true
	).

execute(X) :-
	% TODO: do exceptions work with threads?
	( catch(execute_(X), E, show_exception(E)) ->
%	( execute_(X) ->
	    true
	; display('No'), nl
	).

execute_(X) :-
	( X = [Lib] ->
	    display('Using module \''),
	    write(Lib),
	    display('\'.'),
	    nl,
	    use_module(Lib)
	; call(X)
	).

show_exception(E) :-
	display('Exception: '), write(E), nl.

loop :-
	display('Ok'),
	nl,
	fail.
