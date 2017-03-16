% * Declarative IO with contexts.
%
% This module defines the IO context, that ensures that all (defined)
% IO operations have a declative meaning:
%
%   - no backtracking on the 'real world' is performed.
%
%   - (A,B) is not necessarily equivalent to (B,A) if goals A and B
%     are defined under the 'io' context.
%
% ** How does it work?
%
% It includes a 'real_world_cp' context variable base choice point,
% and each time a IO operation is done, it checks that we do not have
% any pending choice point. To control that non-commutativity of IO
% goals, the 'hipair(io_state)' context is added.
%
% ** Limitations
%
% An extra argument is actually passed ('real_world_cp'). Efficient
% implementations could hide it in the abstract machine itself
% (e.g. by means of a (real) global variable, since it is ground and
% variable sharing is not a problem).
%
% Author: Jose F. Morales

:- module(_, [main/0], [compiler(complang)]).

% ---------------------------------------------------------------------------
% Here starts the example declarative-IO library
%
% Note that this code should be considered as low-level code (and
% non-pure).

:- '$def_binder'(io_enter, (goals_around('$metachoice'(CP), true), push(real_world_cp, CP), push_hipair(io_state))).

:- '$context'(act_on_real_world/0, io).
:- '$incctx'(single(real_world_cp)).
:- '$incctx'(hipair(io_state)).
act_on_real_world :-
	'$metachoice'(CP),
	( CP = ~real_world_cp -> true
	; display('{error: a backtrackable-operation is about to be performed on the Real World, aborting}\n'),
	  fail % exception? halt?
	).

:- '$context'(pure_display/1, io).
pure_display(X) :-
	act_on_real_world,
	display(X).

% this should belong to booting process
main :-
	( io_enter(main__2) ->
	    true
	; display('{error: the program failed on the Real World, aborting}\n'),
	  halt
	).

% ---------------------------------------------------------------------------
% Here starts a small tests program

:- '$begin_context'(io).
main__2 :-
	test1,
	( test2 -> true ; true ),
	fail.

test1 :-
	pure_display('hello world\n').

test2 :-
	( pure_display('bad hello world\n')
	; true
	).
:- '$end'.
